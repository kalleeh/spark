mod lua_api;
mod editor;
mod audio_output;
mod png_cart;

use macroquad::prelude::*;
use spark_core::console::Console;
use spark_core::audio::Audio;
use spark_core::game_state::GameState;

use editor::{Editor, EditorAction};
use audio_output::AudioOutput;

/// Load a cart from a file path, detecting the format by extension.
///
/// Files ending in `.p8.png` are treated as PNG-encoded carts (steganographic
/// data). Everything else is treated as a `.p8` text cart.
fn load_cart_from_file(path: &str) -> Result<spark_core::cart::CartData, String> {
    if path.ends_with(".p8.png") {
        let bytes = std::fs::read(path).map_err(|e| format!("Failed to read file: {}", e))?;
        png_cart::parse_p8_png(&bytes)
    } else {
        let source =
            std::fs::read_to_string(path).map_err(|e| format!("Failed to read file: {}", e))?;
        Ok(spark_core::cart::parse_cart(&source))
    }
}

/// Apply loaded cart data to the editor, console, and audio state.
fn apply_cart_data(
    cart_data: &spark_core::cart::CartData,
    editor: &mut Editor,
    console: &mut Console,
    audio: &mut Audio,
) {
    editor.code = cart_data.code.clone();
    console.sprites = cart_data.sprites;
    console.map = cart_data.map;
    console.flags = cart_data.flags;
    audio.sfx_data = cart_data.sfx;
    audio.music_data = cart_data.music;
    editor.sfx_data = cart_data.sfx;
    editor.music_data = cart_data.music;
}

#[derive(PartialEq)]
enum AppMode {
    Editor,
    Running,
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

#[macroquad::main(window_conf)]
async fn main() {
    let mut console = Console::new();
    let mut editor = Editor::new();
    let mut audio = Audio::new();
    let mut audio_output = AudioOutput::new();
    let mut mode = AppMode::Editor;
    let mut lua: Option<mlua::Lua> = None;
    let mut game_state: Option<GameState> = None;
    let mut error_msg: Option<String> = None;
    let mut cart_path: Option<String> = None;
    let mut use_60fps = false;

    // Create texture for the 128x128 screen
    let image = Image::gen_image_color(128, 128, BLACK);
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
        // Calculate scaling to fit 128x128 in window with aspect ratio
        let sw = screen_width();
        let sh = screen_height();
        let scale = (sw / 128.0).min(sh / 128.0).floor().max(1.0);
        let offset_x = (sw - 128.0 * scale) / 2.0;
        let offset_y = (sh - 128.0 * scale) / 2.0;

        match mode {
            AppMode::Editor => {
                // Update editor screen info for mouse coordinate conversion
                editor.screen_offset_x = offset_x;
                editor.screen_offset_y = offset_y;
                editor.screen_scale = scale;

                // Run editor
                let mut action = editor.update(&mut console);

                // Auto-run on first frame if --run flag was passed
                if first_frame && auto_run {
                    action = EditorAction::RunGame;
                    first_frame = false;
                }

                match action {
                    EditorAction::RunGame => {
                        // Sync editor SFX data to audio before running
                        audio.sfx_data = editor.sfx_data;
                        audio.music_data = editor.music_data;

                        // Create Lua state and run the game
                        match lua_api::create_lua() {
                            Ok(l) => {
                                l.set_app_data(console);
                                l.set_app_data(audio);
                                let gs = GameState::new();
                                l.set_app_data(gs);

                                // Load and execute code
                                let code = editor.code.clone();
                                match lua_api::load_code(&l, &code) {
                                    Ok(()) => {
                                        // Call _init
                                        if let Err(e) = lua_api::call_init(&l) {
                                            error_msg = Some(format!("_init error: {}", e));
                                            console = l.remove_app_data::<Console>()
                                                .unwrap_or_else(Console::new);
                                            audio = l.remove_app_data::<Audio>()
                                                .unwrap_or_else(Audio::new);
                                            let _ = l.remove_app_data::<GameState>();
                                            let _ = l.remove_app_data::<lua_api::Breadcrumb>();
                                            game_state = None;
                                        } else {
                                            // Detect 60fps mode before removing app data
                                            use_60fps = lua_api::has_update60(&l);
                                            if use_60fps {
                                                eprintln!("[spark] 60fps mode detected (_update60 defined)");
                                            }

                                            // Retrieve state back from Lua
                                            console = l.remove_app_data::<Console>()
                                                .unwrap_or_else(Console::new);
                                            audio = l.remove_app_data::<Audio>()
                                                .unwrap_or_else(Audio::new);
                                            game_state = l.remove_app_data::<GameState>();

                                            lua = Some(l);
                                            mode = AppMode::Running;
                                            error_msg = None;
                                        }
                                    }
                                    Err(e) => {
                                        error_msg = Some(format!("Load error: {}", e));
                                        console = l.remove_app_data::<Console>()
                                            .unwrap_or_else(Console::new);
                                        audio = l.remove_app_data::<Audio>()
                                            .unwrap_or_else(Audio::new);
                                        let _ = l.remove_app_data::<GameState>();
                                        let _ = l.remove_app_data::<lua_api::Breadcrumb>();
                                        game_state = None;
                                    }
                                }
                            }
                            Err(e) => {
                                error_msg = Some(format!("Lua error: {}", e));
                            }
                        }
                    }
                    EditorAction::SaveCart => {
                        // Sync editor SFX data to audio before saving
                        audio.sfx_data = editor.sfx_data;
                        audio.music_data = editor.music_data;

                        if let Some(ref path) = cart_path {
                            let cart_data = spark_core::cart::CartData {
                                code: editor.code.clone(),
                                sprites: console.sprites,
                                map: console.map,
                                flags: console.flags,
                                sfx: editor.sfx_data,
                                music: editor.music_data,
                            };
                            let text = spark_core::cart::serialize_cart(&cart_data);
                            match std::fs::write(path, &text) {
                                Ok(()) => {
                                    eprintln!("[spark] saved cart to {}", path);
                                    error_msg = Some(format!("Saved to {}", path));
                                }
                                Err(e) => {
                                    error_msg = Some(format!("Save error: {}", e));
                                }
                            }
                        } else {
                            error_msg = Some("No cart path -- load a .p8 file first".to_string());
                        }
                    }
                    EditorAction::SaveCartAs => {
                        // Sync editor SFX data to audio before saving
                        audio.sfx_data = editor.sfx_data;
                        audio.music_data = editor.music_data;

                        let save_path = cart_path
                            .clone()
                            .unwrap_or_else(|| "untitled.p8".to_string());
                        let cart_data = spark_core::cart::CartData {
                            code: editor.code.clone(),
                            sprites: console.sprites,
                            map: console.map,
                            flags: console.flags,
                            sfx: editor.sfx_data,
                            music: editor.music_data,
                        };
                        let text = spark_core::cart::serialize_cart(&cart_data);
                        match std::fs::write(&save_path, &text) {
                            Ok(()) => {
                                eprintln!("[spark] saved cart to {}", save_path);
                                error_msg = Some(format!("Saved to {}", save_path));
                                cart_path = Some(save_path);
                            }
                            Err(e) => {
                                error_msg = Some(format!("Save error: {}", e));
                            }
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
                                    apply_cart_data(&cart_data, &mut editor, &mut console, &mut audio);
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
                if let Some(ref l) = lua {
                    // Gather input from keyboard
                    let btn_state = [
                        // Player 1: arrow keys + Z/C/N for O, X/V/M for X
                        is_key_down(KeyCode::Left),
                        is_key_down(KeyCode::Right),
                        is_key_down(KeyCode::Up),
                        is_key_down(KeyCode::Down),
                        is_key_down(KeyCode::Z) || is_key_down(KeyCode::C) || is_key_down(KeyCode::N),
                        is_key_down(KeyCode::X) || is_key_down(KeyCode::V) || is_key_down(KeyCode::M),
                        // Player 2: ESDF for dpad, LShift for O, A for X
                        is_key_down(KeyCode::S),          // P2 left
                        is_key_down(KeyCode::F),          // P2 right
                        is_key_down(KeyCode::E),          // P2 up
                        is_key_down(KeyCode::D),          // P2 down
                        is_key_down(KeyCode::LeftShift),  // P2 O button
                        is_key_down(KeyCode::A),          // P2 X button
                    ];
                    console.update_input(btn_state);

                    // Wire mouse position (convert window coords to 128x128 game coords)
                    let (mx, my) = mouse_position();
                    console.mouse_x = ((mx - offset_x) / scale) as i32;
                    console.mouse_y = ((my - offset_y) / scale) as i32;
                    console.mouse_btn =
                        if is_mouse_button_down(MouseButton::Left) { 1 } else { 0 }
                        | if is_mouse_button_down(MouseButton::Right) { 2 } else { 0 }
                        | if is_mouse_button_down(MouseButton::Middle) { 4 } else { 0 };

                    // Wire keyboard input for stat(30)/stat(31)
                    let key_char = get_char_pressed();
                    console.key_pressed = key_char.is_some();
                    console.last_key_char = key_char;

                    // Set app data for Lua callbacks
                    l.set_app_data(console);
                    l.set_app_data(audio);
                    l.set_app_data(game_state.take().unwrap_or_else(GameState::new));

                    // Call _update or _update60 (increments frame_count) and _draw
                    let update_result = if use_60fps {
                        lua_api::call_update60(l)
                    } else {
                        lua_api::call_update(l)
                    };

                    // Check for load() signal in update errors
                    let mut load_request: Option<(String, String)> = None;
                    if let Err(e) = update_result {
                        let err_str = format!("{}", e);
                        if let Some(parsed) = lua_api::parse_load_signal(&err_str) {
                            load_request = Some(parsed);
                        } else {
                            let msg = format!("Update error: {}", e);
                            eprintln!("[spark] {}", msg);
                            error_msg = Some(msg);
                        }
                    }
                    if error_msg.is_none() && load_request.is_none() {
                        if let Err(e) = lua_api::call_draw(l) {
                            let err_str = format!("{}", e);
                            if let Some(parsed) = lua_api::parse_load_signal(&err_str) {
                                load_request = Some(parsed);
                            } else {
                                let msg = format!("Draw error: {}", e);
                                eprintln!("[spark] {}", msg);
                                error_msg = Some(msg);
                            }
                        }
                    }

                    // Retrieve state back from Lua
                    console = l.remove_app_data::<Console>()
                        .unwrap_or_else(Console::new);
                    audio = l.remove_app_data::<Audio>()
                        .unwrap_or_else(Audio::new);
                    game_state = l.remove_app_data::<GameState>();
                    // Note: Breadcrumb stays in Lua app_data -- it persists
                    // for the lifetime of this Lua state (for stat(6) access).

                    // Handle load() request: tear down current state and load new cart
                    if let Some((ref filename, ref breadcrumb)) = load_request {
                        eprintln!("[spark] load() requested: {:?} breadcrumb={:?}", filename, breadcrumb);

                        // Resolve path relative to current cart's directory
                        let resolved_path = if let Some(ref current) = cart_path {
                            let current_dir = std::path::Path::new(current)
                                .parent()
                                .unwrap_or_else(|| std::path::Path::new("."));
                            let candidate = current_dir.join(filename);
                            candidate.to_string_lossy().into_owned()
                        } else {
                            filename.clone()
                        };

                        // Append .p8 extension if missing (unless it already has .p8.png)
                        let resolved_path = if resolved_path.ends_with(".p8")
                            || resolved_path.ends_with(".p8.png")
                        {
                            resolved_path
                        } else {
                            format!("{}.p8", resolved_path)
                        };

                        match load_cart_from_file(&resolved_path) {
                            Ok(cart_data) => {
                                // Drop old Lua state
                                lua = None;
                                game_state = None;

                                // Load new cart data into console and audio
                                console = Console::new();
                                audio = Audio::new();
                                apply_cart_data(&cart_data, &mut editor, &mut console, &mut audio);
                                cart_path = Some(resolved_path.clone());

                                // Create fresh Lua state for the new cart
                                match lua_api::create_lua() {
                                    Ok(new_l) => {
                                        new_l.set_app_data(console);
                                        new_l.set_app_data(audio);
                                        let gs = GameState::new();
                                        new_l.set_app_data(gs);
                                        new_l.set_app_data(lua_api::Breadcrumb(breadcrumb.clone()));

                                        match lua_api::load_code(&new_l, &cart_data.code) {
                                            Ok(()) => {
                                                if let Err(e) = lua_api::call_init(&new_l) {
                                                    // Check if _init itself calls load()
                                                    let init_err = format!("{}", e);
                                                    if lua_api::parse_load_signal(&init_err).is_some() {
                                                        // _init called load() -- retrieve state
                                                        // and let the next iteration handle it
                                                        // by setting error_msg to the signal.
                                                        // This is a rare edge case.
                                                        error_msg = Some(format!("_init error: {}", e));
                                                    } else {
                                                        error_msg = Some(format!("_init error: {}", e));
                                                    }
                                                    console = new_l.remove_app_data::<Console>()
                                                        .unwrap_or_else(Console::new);
                                                    audio = new_l.remove_app_data::<Audio>()
                                                        .unwrap_or_else(Audio::new);
                                                    let _ = new_l.remove_app_data::<GameState>();
                                                    let _ = new_l.remove_app_data::<lua_api::Breadcrumb>();
                                                    game_state = None;
                                                } else {
                                                    use_60fps = lua_api::has_update60(&new_l);
                                                    if use_60fps {
                                                        eprintln!("[spark] 60fps mode detected (_update60 defined)");
                                                    }
                                                    console = new_l.remove_app_data::<Console>()
                                                        .unwrap_or_else(Console::new);
                                                    audio = new_l.remove_app_data::<Audio>()
                                                        .unwrap_or_else(Audio::new);
                                                    game_state = new_l.remove_app_data::<GameState>();
                                                    lua = Some(new_l);
                                                    error_msg = None;
                                                    eprintln!("[spark] loaded and running: {}", resolved_path);
                                                }
                                            }
                                            Err(e) => {
                                                error_msg = Some(format!("Load error: {}", e));
                                                console = new_l.remove_app_data::<Console>()
                                                    .unwrap_or_else(Console::new);
                                                audio = new_l.remove_app_data::<Audio>()
                                                    .unwrap_or_else(Audio::new);
                                                let _ = new_l.remove_app_data::<GameState>();
                                                let _ = new_l.remove_app_data::<lua_api::Breadcrumb>();
                                                game_state = None;
                                            }
                                        }
                                    }
                                    Err(e) => {
                                        error_msg = Some(format!("Lua error: {}", e));
                                    }
                                }
                            }
                            Err(e) => {
                                error_msg = Some(format!("load() error: {} not found: {}", resolved_path, e));
                                eprintln!("[spark] load() failed: {}", e);
                            }
                        }
                    }
                } else {
                    mode = AppMode::Editor;
                    console.reset_draw_state();
                }

                // Update audio state machine (advances SFX/music note positions).
                audio.update();

                // Generate this frame's audio samples and play them through
                // the system audio output. At 30fps this produces ~1470 samples
                // of mono PCM at 44100 Hz; at 60fps it produces ~735 samples
                // (half per frame since frames come twice as fast).
                audio_output.play_frame_audio_with_fps(&mut audio, use_60fps).await;

                // Escape to return to editor, or on error
                if is_key_pressed(KeyCode::Escape) || error_msg.is_some() {
                    // Stop any playing audio before returning to editor.
                    audio_output.stop();
                    mode = AppMode::Editor;
                    lua = None;
                    game_state = None;
                    use_60fps = false;
                    console.reset_draw_state();
                    // Sync audio data back to editor for the SFX tab
                    editor.sfx_data = audio.sfx_data;
                    editor.music_data = audio.music_data;
                }
            }
        }

        // Render the 128x128 screen buffer to the window
        let rgba = console.screen_to_rgba();
        let img = Image {
            bytes: rgba,
            width: 128,
            height: 128,
        };
        texture.update(&img);

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

        // Draw error message overlay if any (using macroquad's built-in text)
        if let Some(ref msg) = error_msg {
            draw_text(msg, 4.0, sh - 8.0, 16.0, RED);
        }

        next_frame().await;
    }
}
