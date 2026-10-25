mod audio_output;
mod editor;

use macroquad::prelude::*;
use spark_core::audio::Audio;
use spark_core::cart::CartData;
use spark_core::console::Console;
use spark_desktop_lib::{install_cart, load_cart_from_file, CartRuntime, Tick};

use audio_output::AudioOutput;
use editor::{Editor, EditorAction};

/// Apply loaded cart data to the editor, console, and audio state.
fn apply_cart_data(cart: &CartData, editor: &mut Editor, console: &mut Console, audio: &mut Audio) {
    editor.code = cart.code.clone();
    install_cart(console, audio, cart);
}

/// Assemble a cart from what the editor currently holds.
fn cart_from_editor(editor: &Editor, console: &Console, audio: &Audio) -> CartData {
    CartData {
        code: editor.code.clone(),
        sprites: console.sprites,
        map: console.map,
        flags: console.flags,
        sfx: audio.sfx_data,
        music: audio.music_data,
    }
}

/// Serialize the editor's cart to `path` as a `.p8` text cart.
fn save_cart(path: &str, editor: &Editor, console: &Console, audio: &Audio) -> Result<(), String> {
    let text = spark_core::cart::serialize_cart(&cart_from_editor(editor, console, audio));
    std::fs::write(path, &text).map_err(|e| format!("Save error: {}", e))?;
    eprintln!("[spark] saved cart to {}", path);
    Ok(())
}

/// Resolve a `load()` filename: relative to the current cart's directory, with
/// `.p8` appended unless it already names a `.p8`/`.p8.png` file.
fn resolve_load_path(filename: &str, current_cart: Option<&str>) -> String {
    let path = match current_cart {
        Some(current) => std::path::Path::new(current)
            .parent()
            .unwrap_or_else(|| std::path::Path::new("."))
            .join(filename)
            .to_string_lossy()
            .into_owned(),
        None => filename.to_string(),
    };
    if path.ends_with(".p8") || path.ends_with(".p8.png") {
        path
    } else {
        format!("{}.p8", path)
    }
}

/// PICO-8 button state from the keyboard: P1 arrows + Z/C/N (O) + X/V/M (X);
/// P2 ESDF + LShift (O) + A (X).
fn read_buttons() -> [bool; 12] {
    [
        is_key_down(KeyCode::Left),
        is_key_down(KeyCode::Right),
        is_key_down(KeyCode::Up),
        is_key_down(KeyCode::Down),
        is_key_down(KeyCode::Z) || is_key_down(KeyCode::C) || is_key_down(KeyCode::N),
        is_key_down(KeyCode::X) || is_key_down(KeyCode::V) || is_key_down(KeyCode::M),
        is_key_down(KeyCode::S),
        is_key_down(KeyCode::F),
        is_key_down(KeyCode::E),
        is_key_down(KeyCode::D),
        is_key_down(KeyCode::LeftShift),
        is_key_down(KeyCode::A),
    ]
}

/// `stat(34)` mouse button bitmask (1=left, 2=right, 4=middle).
fn mouse_mask() -> u8 {
    (is_mouse_button_down(MouseButton::Left) as u8)
        | ((is_mouse_button_down(MouseButton::Right) as u8) << 1)
        | ((is_mouse_button_down(MouseButton::Middle) as u8) << 2)
}

#[derive(Clone, Copy, PartialEq)]
enum AppMode {
    Editor,
    Running,
    Paused,
}

/// A running cart plus what is needed to restart it on `run()`.
struct Game {
    rt: CartRuntime,
    cart: CartData,
    breadcrumb: String,
    param: String,
}

impl Game {
    fn start(cart: CartData, breadcrumb: String, param: String) -> Result<Game, String> {
        let rt = CartRuntime::start(&cart, &breadcrumb, &param)?;
        if rt.use_60fps() {
            eprintln!("[spark] 60fps mode detected (_update60 defined)");
        }
        if rt.is_coroutine_mode() {
            eprintln!("[spark] coroutine mode (manual loop detected)");
        } else {
            eprintln!("[spark] callback mode");
        }
        Ok(Game {
            rt,
            cart,
            breadcrumb,
            param,
        })
    }
}

/// How the Running/Paused branches ask the frame to leave the current cart.
enum Exit {
    /// Back to the editor (stop(), escape, quit, error).
    ToEditor,
    /// `run()`: restart the same cart.
    Restart,
    /// `load()`: switch to another cart.
    Load {
        file: String,
        breadcrumb: String,
        param: String,
    },
}

fn window_conf() -> Conf {
    Conf {
        window_title: "Spark - Fantasy Console".to_owned(),
        window_width: 512,
        window_height: 512,
        window_resizable: true,
        ..Default::default()
    }
}

/// Cap on catch-up ticks per window frame so a hitch can't run many frames at once.
const MAX_CATCHUP_TICKS: u32 = 4;

#[macroquad::main(window_conf)]
async fn main() {
    // Editor-mode engine state. While a cart runs, its own Console/Audio live
    // inside `game` (resident in the Lua state) and these are left untouched.
    let mut console = Console::new();
    let mut audio = Audio::new();
    let mut editor = Editor::new();
    let mut audio_output = AudioOutput::new();
    let mut mode = AppMode::Editor;
    let mut game: Option<Game> = None;
    let mut error_msg: Option<String> = None;
    let mut cart_path: Option<String> = None;
    let mut pause_selection: usize = 0; // 0=Continue, 1..=N=items, last=Quit

    // Fixed-timestep accumulator for game logic. The window may refresh far
    // faster than the cart's tick rate (e.g. a 120 Hz display), so we advance
    // `_update`/`_draw` only once enough wall-clock time has accumulated for one
    // cart frame (1/30 or 1/60 s) — otherwise the game runs at monitor speed.
    let mut frame_accumulator: f64 = 0.0;

    // Create texture for the 128x128 screen. `image.bytes` is reused as the
    // RGBA scratch buffer every frame (see the render section) to avoid a
    // ~65 KB heap allocation per frame.
    let mut image = Image::gen_image_color(128, 128, BLACK);
    let texture = Texture2D::from_image(&image);
    texture.set_filter(FilterMode::Nearest); // Pixel-perfect scaling

    // Load cart from command line args if provided
    let args: Vec<String> = std::env::args().collect();
    let mut auto_run = false;
    if args.len() > 1 {
        let path = &args[1];
        // --run flag to auto-run
        if args.iter().any(|a| a == "--run" || a == "-r") {
            auto_run = true;
        }
        let found_path = args
            .iter()
            .find(|a| a.ends_with(".p8") || a.ends_with(".p8.png"))
            .unwrap_or(path);
        match load_cart_from_file(found_path) {
            Ok(cart_data) => {
                apply_cart_data(&cart_data, &mut editor, &mut console, &mut audio);
                cart_path = Some(found_path.clone());
                if auto_run {
                    eprintln!("[spark] auto-running cart: {}", found_path);
                }
            }
            Err(e) => {
                error_msg = Some(format!("Failed to load cart: {}", e));
            }
        }
    }
    let mut first_frame = true;

    loop {
        // Advance the fixed-timestep accumulator by real elapsed time. Clamp to
        // avoid a "spiral of death" after a long stall (e.g. a slow first frame
        // or the window being backgrounded) — at most a few catch-up ticks.
        let dt = (macroquad::time::get_frame_time() as f64).min(0.25);
        frame_accumulator += dt;

        // Calculate scaling to fit 128x128 in window with aspect ratio
        let sw = screen_width();
        let sh = screen_height();
        let scale = (sw / 128.0).min(sh / 128.0).floor().max(1.0);
        let offset_x = (sw - 128.0 * scale) / 2.0;
        let offset_y = (sh - 128.0 * scale) / 2.0;

        let mode_at_start = mode;
        let mut ticks_this_frame = 0u32;
        let mut exit: Option<Exit> = None;
        let mut pause_menu: Vec<String> = Vec::new();

        match mode {
            AppMode::Editor => {
                // Update editor screen info for mouse coordinate conversion
                editor.screen_offset_x = offset_x;
                editor.screen_offset_y = offset_y;
                editor.screen_scale = scale;

                // Run editor
                let mut action = editor.update(&mut console, &mut audio);

                // Auto-run on first frame if --run flag was passed
                if first_frame && auto_run {
                    action = EditorAction::RunGame;
                }
                first_frame = false;

                match action {
                    EditorAction::RunGame => {
                        let cart = cart_from_editor(&editor, &console, &audio);
                        match Game::start(cart, String::new(), String::new()) {
                            Ok(g) => {
                                game = Some(g);
                                mode = AppMode::Running;
                                error_msg = None;
                                frame_accumulator = 0.0;
                            }
                            Err(e) => error_msg = Some(e),
                        }
                    }
                    EditorAction::SaveCart => match &cart_path {
                        Some(path) => {
                            error_msg = Some(match save_cart(path, &editor, &console, &audio) {
                                Ok(()) => format!("Saved to {}", path),
                                Err(e) => e,
                            });
                        }
                        None => {
                            error_msg = Some("No cart path -- load a .p8 file first".to_string());
                        }
                    },
                    EditorAction::SaveCartAs => {
                        let save_path = cart_path
                            .clone()
                            .unwrap_or_else(|| "untitled.p8".to_string());
                        match save_cart(&save_path, &editor, &console, &audio) {
                            Ok(()) => {
                                error_msg = Some(format!("Saved to {}", save_path));
                                cart_path = Some(save_path);
                            }
                            Err(e) => error_msg = Some(e),
                        }
                    }
                    EditorAction::NewCart => {
                        // Reset everything to defaults
                        editor.reset_to_defaults();
                        console = Console::new();
                        audio = Audio::new();
                        cart_path = None;
                        error_msg = Some("New cart created".to_string());
                        eprintln!("[spark] new cart");
                    }
                    EditorAction::LoadCart => {
                        if let Some(path) = editor.pending_load.take() {
                            match load_cart_from_file(&path) {
                                Ok(cart_data) => {
                                    apply_cart_data(
                                        &cart_data,
                                        &mut editor,
                                        &mut console,
                                        &mut audio,
                                    );
                                    cart_path = Some(path.clone());
                                    error_msg = Some(format!("Loaded {}", path));
                                    eprintln!("[spark] loaded cart: {}", path);
                                }
                                Err(e) => {
                                    error_msg = Some(format!("Load error: {}", e));
                                }
                            }
                        }
                    }
                    EditorAction::None => {}
                }
            }

            AppMode::Running => {
                let Some(g) = game.as_mut() else {
                    mode = AppMode::Editor;
                    continue;
                };

                // Run game logic at the cart's fixed tick rate (30 or 60 Hz),
                // independent of the window refresh rate. Consume whole frame
                // periods from the accumulator; cap catch-up ticks so a hitch
                // can't make us run many frames at once.
                let frame_period = g.rt.frame_period();
                while frame_accumulator >= frame_period
                    && ticks_this_frame < MAX_CATCHUP_TICKS
                    && exit.is_none()
                {
                    frame_accumulator -= frame_period;
                    ticks_this_frame += 1;

                    // Wire input straight into the resident Console.
                    {
                        let mut con = g.rt.console_mut();
                        con.update_input(read_buttons());
                        // Mouse position in 128x128 game coordinates.
                        let (mx, my) = mouse_position();
                        con.mouse_x = ((mx - offset_x) / scale) as i32;
                        con.mouse_y = ((my - offset_y) / scale) as i32;
                        con.mouse_btn = mouse_mask();
                        // Keyboard input for stat(30)/stat(31).
                        let key_char = get_char_pressed();
                        con.key_pressed = key_char.is_some();
                        con.last_key_char = key_char;
                    }
                    g.rt.game_state_mut().actual_fps = macroquad::time::get_fps() as f32;

                    match g.rt.tick() {
                        Tick::Continue => {}
                        Tick::Stop => exit = Some(Exit::ToEditor),
                        Tick::Restart => exit = Some(Exit::Restart),
                        Tick::Load {
                            file,
                            breadcrumb,
                            param,
                        } => {
                            exit = Some(Exit::Load {
                                file,
                                breadcrumb,
                                param,
                            })
                        }
                        Tick::Error(msg) => {
                            eprintln!("[spark] {}", msg);
                            error_msg = Some(msg);
                            exit = Some(Exit::ToEditor);
                        }
                    }
                }

                // Escape/P pauses (if the cart registered menu items) or exits.
                // Checked every window frame, not only on frames with a tick.
                if exit.is_none() && (is_key_pressed(KeyCode::Escape) || is_key_pressed(KeyCode::P))
                {
                    if g.rt.has_menu_items() {
                        mode = AppMode::Paused;
                        pause_selection = 0;
                    } else {
                        exit = Some(Exit::ToEditor);
                    }
                }
            }

            AppMode::Paused => {
                let Some(g) = game.as_mut() else {
                    mode = AppMode::Editor;
                    continue;
                };
                // Menu: ["Continue", ...items..., "Quit"]; built once, reused
                // by the overlay renderer below.
                pause_menu = g.rt.menu_labels();
                let num_entries = pause_menu.len();

                // Navigation
                if is_key_pressed(KeyCode::Up) && pause_selection > 0 {
                    pause_selection -= 1;
                }
                if is_key_pressed(KeyCode::Down) && pause_selection + 1 < num_entries {
                    pause_selection += 1;
                }

                // Confirm selection
                let confirmed = is_key_pressed(KeyCode::Enter)
                    || is_key_pressed(KeyCode::Z)
                    || is_key_pressed(KeyCode::X);

                if confirmed {
                    if pause_selection == 0 {
                        mode = AppMode::Running;
                    } else if pause_selection + 1 == num_entries {
                        exit = Some(Exit::ToEditor);
                    } else {
                        // Custom item: a truthy return keeps the menu open.
                        let keep_open = g.rt.call_menu_item(pause_selection - 1);
                        mode = if keep_open {
                            AppMode::Paused
                        } else {
                            AppMode::Running
                        };
                    }
                }

                // Dismiss with Escape (back to game)
                if is_key_pressed(KeyCode::Escape) {
                    mode = AppMode::Running;
                }
            }
        }

        // Leave the current cart: tear the runtime down (saving cartdata) and
        // either return to the editor or start the next cart.
        if let Some(exit) = exit {
            if let Some(g) = game.take() {
                pause_selection = 0;
                let next: Result<Option<Game>, String> = match exit {
                    Exit::ToEditor => {
                        // The editor keeps the cart's runtime sset/mset edits,
                        // as PICO-8 does.
                        let (c, a) = g.rt.finish();
                        console = c;
                        audio = a;
                        Ok(None)
                    }
                    Exit::Restart => {
                        g.rt.finish();
                        Game::start(g.cart, g.breadcrumb, g.param).map(Some)
                    }
                    Exit::Load {
                        file,
                        breadcrumb,
                        param,
                    } => {
                        eprintln!(
                            "[spark] load() requested: {:?} breadcrumb={:?} param={:?}",
                            file, breadcrumb, param
                        );
                        let resolved = resolve_load_path(&file, cart_path.as_deref());
                        g.rt.finish();
                        match load_cart_from_file(&resolved) {
                            Ok(cart) => {
                                apply_cart_data(&cart, &mut editor, &mut console, &mut audio);
                                cart_path = Some(resolved.clone());
                                eprintln!("[spark] loaded and running: {}", resolved);
                                Game::start(cart, breadcrumb, param).map(Some)
                            }
                            Err(e) => Err(format!("load() error: {} not found: {}", resolved, e)),
                        }
                    }
                };
                match next {
                    Ok(Some(g)) => {
                        game = Some(g);
                        mode = AppMode::Running;
                        error_msg = None;
                        frame_accumulator = 0.0;
                    }
                    Ok(None) => {
                        console.reset_draw_state();
                        audio_output.stop();
                        mode = AppMode::Editor;
                    }
                    Err(e) => {
                        eprintln!("[spark] {}", e);
                        error_msg = Some(e);
                        console.reset_draw_state();
                        audio_output.stop();
                        mode = AppMode::Editor;
                    }
                }
            }
        }

        // Pump audio in REAL TIME, decoupled from the fixed game-tick rate.
        // Game logic steps at 30/60 Hz, but the audio engine must emit a
        // continuous stream at the system sample rate or playback stutters.
        // We generate one chunk sized to this visual frame's elapsed time `dt`,
        // every visual frame, only while a cart is running.
        if mode == AppMode::Running {
            if let Some(g) = &game {
                audio_output.play_realtime_audio(&mut g.rt.audio_mut(), dt);
            }
        }

        // Render the 128x128 screen buffer to the window. The GPU upload is
        // skipped on window frames where nothing could have changed: no cart
        // tick ran and we are still in the same mode (the editor redraws every
        // frame, so Editor mode always uploads).
        let screen_dirty = ticks_this_frame > 0 || mode == AppMode::Editor || mode != mode_at_start;
        if screen_dirty {
            match &game {
                Some(g) if mode != AppMode::Editor => {
                    g.rt.console().screen_to_rgba_buf(&mut image.bytes)
                }
                _ => console.screen_to_rgba_buf(&mut image.bytes),
            }
            texture.update(&image);
        }

        clear_background(Color::new(0.1, 0.1, 0.1, 1.0));
        draw_texture_ex(
            &texture,
            offset_x,
            offset_y,
            WHITE,
            DrawTextureParams {
                dest_size: Some(vec2(128.0 * scale, 128.0 * scale)),
                ..Default::default()
            },
        );

        // Draw pause overlay if paused
        if mode == AppMode::Paused {
            if pause_menu.is_empty() {
                // Entered Paused this frame (from Running): build the list once.
                if let Some(g) = &game {
                    pause_menu = g.rt.menu_labels();
                }
            }
            let overlay_w = 80.0 * scale;
            let overlay_h = 60.0 * scale;
            let overlay_x = (sw - overlay_w) / 2.0;
            let overlay_y = (sh - overlay_h) / 2.0;
            draw_rectangle(
                overlay_x,
                overlay_y,
                overlay_w,
                overlay_h,
                Color::new(0.0, 0.0, 0.0, 0.85),
            );

            let text_x = overlay_x + 4.0 * scale;
            let font_size = 8.0 * scale;

            draw_text(
                "PAUSED",
                text_x,
                overlay_y + font_size * 1.5,
                font_size,
                WHITE,
            );

            for (i, entry) in pause_menu.iter().enumerate() {
                let y = overlay_y + font_size * (3.0 + i as f32 * 1.5);
                let color = if i == pause_selection { YELLOW } else { WHITE };
                let prefix = if i == pause_selection { "> " } else { "  " };
                draw_text(&format!("{}{}", prefix, entry), text_x, y, font_size, color);
            }
        }

        // Draw error message overlay if any (using macroquad's built-in text)
        if let Some(ref msg) = error_msg {
            draw_text(msg, 4.0, sh - 8.0, 16.0, RED);
        }

        next_frame().await;
    }
}
