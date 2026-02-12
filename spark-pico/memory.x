MEMORY {
    /*
     * RP2350 has 520KB SRAM starting at 0x20000000.
     * FLASH: 2MB starting at 0x10000000 (typical for Pico 2 boards).
     */
    FLASH : ORIGIN = 0x10000000, LENGTH = 2048K
    RAM   : ORIGIN = 0x20000000, LENGTH = 520K
}

SECTIONS {
    /* RP2350 boot info block (IMAGE_DEF) — must appear in the first 4KB */
    .start_block :
    {
        __start_block_addr = .;
        KEEP(*(.start_block));
    } > FLASH

    /* Binary info entries for picotool */
    .bi_entries :
    {
        __bi_entries_start = .;
        KEEP(*(.bi_entries));
        __bi_entries_end = .;
    } > FLASH
} INSERT AFTER .rodata;
