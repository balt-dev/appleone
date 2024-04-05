// TODO: ACIE

#![warn(clippy::pedantic, clippy::perf)]
#![allow(clippy::too_many_lines)]

mod constants;

use crossterm::{
    cursor::{EnableBlinking, MoveTo, MoveToNextLine},
    event::{poll, read, Event, KeyCode, KeyEvent, KeyEventKind},
    execute,
    style::Print,
    terminal::{
        disable_raw_mode, enable_raw_mode, Clear, ClearType, DisableLineWrap, EnableLineWrap,
        EnterAlternateScreen, LeaveAlternateScreen,
    },
};
use r6502::{Emulator, FunctionReadCallback, FunctionWriteCallback, Opcode, State};
use std::{
    cell::RefCell,
    fs, io,
    path::PathBuf,
    process::ExitCode,
    rc::Rc,
    time::{Duration, Instant},
};

fn main() -> ExitCode {
    let Err(err) = inner_main() else {
        disable_raw_mode().expect("failed to disable raw mode");
        execute!(io::stdout(), LeaveAlternateScreen, EnableLineWrap)
            .expect("failed to clear alternate screen");
        return ExitCode::SUCCESS;
    };
    disable_raw_mode().expect("failed to disable raw mode");
    execute!(io::stdout(), LeaveAlternateScreen, EnableLineWrap)
        .expect("failed to clear alternate screen");
    eprintln!("io error: {err}");
    ExitCode::FAILURE
}

struct EmulatorState {
    keyboard_ready: bool,
    keyboard_register: u8,
    display_x: u8,
    display_y: u8,
    display_reg: u8,
    display_buffer: [u8; 40 * 24],
    old_display_buffer: Option<[u8; 40 * 24]>,
    paused: bool,
    step: bool,
    step_counter: u16,
    last_display: Instant,
}

static CHARACTER_SET: &[u8] = br##"@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_ !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_ !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_ !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_ !"#$%&'()*+,-./0123456789:;<=>?"##;

fn inner_main() -> io::Result<()> {
    execute!(
        io::stdout(),
        EnterAlternateScreen,
        DisableLineWrap,
        EnableBlinking,
        Clear(ClearType::All)
    )?;

    enable_raw_mode()?;

    let emu_state = Rc::new(RefCell::new(EmulatorState {
        keyboard_ready: false,
        keyboard_register: 0b1000_0000,
        display_x: 0,
        display_y: 0,
        display_reg: 0b0000_0000,
        display_buffer: [b' '; 40 * 24],
        old_display_buffer: None,
        paused: false,
        step: false,
        step_counter: 0,
        last_display: Instant::now()
    }));

    let mut emulator = Emulator::default()
        .with_program_counter(0xFF00)
        .with_rom_from(include_bytes!("basic.rom"), 0xE000)
        .with_rom_from(include_bytes!("wozmon.rom"), 0xFF00)
        .with_write_callback(FunctionWriteCallback(
            |state: &mut State, addr, mut byte| {
                if emu_state.borrow_mut().step {
                    let _ = execute!(
                        io::stdout(),
                        Print(format!("WRITE {addr:04X} {byte:02X}")),
                        MoveToNextLine(1)
                    );
                }
                match addr {
                    0..=0x7FFF => state.memory[addr as usize] = byte,
                    constants::variables::DSP => {
                        let mut emu_state = emu_state.borrow_mut();
                        byte &= 0b0111_1111;
                        emu_state.display_reg = byte | 0b1000_0000;
                        emu_state.last_display = Instant::now();
                        match byte {
                            0x00 | 0x7F => {}
                            b'\n' | b'\r' => {
                                emu_state.old_display_buffer = Some(emu_state.display_buffer);
                                emu_state.display_x = 0;
                                emu_state.display_y += 1;
                            }
                            other => {
                                emu_state.old_display_buffer = Some(emu_state.display_buffer);
                                let index = (emu_state.display_y as usize) * 40
                                    + (emu_state.display_x as usize);
                                emu_state.display_buffer[index] = other;
                                emu_state.display_x += 1;
                            }
                        }
                        if emu_state.display_x == 40 {
                            if emu_state.old_display_buffer.is_none() {
                                emu_state.old_display_buffer = Some(emu_state.display_buffer);
                            }
                            emu_state.display_x = 0;
                            emu_state.display_y += 1;
                        }
                        if emu_state.display_y > 23 {
                            if emu_state.old_display_buffer.is_none() {
                                emu_state.old_display_buffer = Some(emu_state.display_buffer);
                            }
                            emu_state.display_y = 23;
                            // Copy the screen buffer up for a new line
                            emu_state.display_buffer.rotate_left(40);
                            emu_state.display_buffer[40 * 23..].fill(b' ');
                        }
                    }
                    _ => {} // ROM
                }
            },
        ))
        .with_read_callback(FunctionReadCallback(|state: &mut State, addr| {
            let res = match addr {
                constants::variables::KBD => {
                    let mut emu_state = emu_state.borrow_mut();
                    emu_state.keyboard_ready = false;
                    emu_state.keyboard_register
                }
                constants::variables::KBD_CR => {
                    if emu_state.borrow_mut().keyboard_ready {
                        0xa7
                    } else {
                        0x00
                    }
                }
                constants::variables::DSP => emu_state.borrow_mut().display_reg,
                constants::variables::DSP_CR => 0b1000_0000,
                _ => state.memory[addr as usize],
            };
            if emu_state.borrow_mut().step {
                let _ = execute!(
                    io::stdout(),
                    Print(format!("READ {addr:04X} {res:02X}")),
                    MoveToNextLine(1)
                );
            }
            res
        }));

    let mut last = Instant::now();

    loop {
        let mut emu_state = emu_state.borrow_mut();
        emu_state.step = false;

        if poll(Duration::ZERO)? {
            let event = read()?;
            if let Event::Key(KeyEvent {
                code,
                kind: KeyEventKind::Press,
                ..
            }) = event
            {
                match keycode_ascii(code) {
                    Ok(ascii) => {
                        emu_state.keyboard_ready = true;
                        emu_state.keyboard_register = ascii | 0b1000_0000;
                    }
                    Err(KeyCode::End | KeyCode::F(1)) => {
                        // Stop
                        break Ok(());
                    }
                    Err(KeyCode::Pause | KeyCode::F(2)) => {
                        // Pause
                        emu_state.paused ^= true;
                        emu_state.step_counter = 0;
                        emu_state.old_display_buffer = Some(emu_state.display_buffer);
                    }
                    Err(KeyCode::Home | KeyCode::F(3)) => {
                        // Reset
                        let reset = u16::from_le_bytes([
                            emulator.state.memory[0xFFFC],
                            emulator.state.memory[0xFFFD],
                        ]);
                        emulator.state.program_counter = reset;
                    }
                    Err(KeyCode::Insert | KeyCode::F(4)) => {
                        // Clear screen
                        emu_state.display_buffer = [b' '; 40 * 24];
                        emu_state.display_x = 0;
                        emu_state.display_y = 0;
                        emu_state.step_counter = 0;
                        emu_state.old_display_buffer = Some([b'@'; 40 * 24]);
                        execute!(io::stdout(), Clear(ClearType::All))?;
                    }
                    Err(KeyCode::F(5)) => {
                        disable_raw_mode()?;

                        // Load a file
                        execute!(
                            io::stdout(),
                            MoveTo(0, 32),
                            EnableLineWrap,
                            Print("File path?")
                        )?;
                        let mut contents = None;
                        while contents.is_none() {
                            execute!(io::stdout(), MoveTo(0, 33), Clear(ClearType::CurrentLine))?;
                            let mut path_str = String::new();
                            io::stdin().read_line(&mut path_str)?;
                            if path_str.trim().is_empty() {
                                break;
                            }
                            let path_buf = PathBuf::from(path_str.trim());
                            if path_buf.exists() {
                                contents = fs::read(path_buf).ok();
                            }
                        }
                        let Some(contents) = contents else {
                            enable_raw_mode()?;
                            continue;
                        };
                        execute!(
                            io::stdout(),
                            MoveTo(0, 34),
                            EnableLineWrap,
                            Print(format!(
                                "Read {} bytes.\nPlace in ROM (in hex):",
                                contents.len()
                            ))
                        )?;
                        let mut placed = false;
                        while !placed {
                            execute!(io::stdout(), MoveTo(0, 36), Clear(ClearType::CurrentLine))?;
                            let mut input = String::new();
                            io::stdin().read_line(&mut input)?;
                            if input.trim().is_empty() {
                                break;
                            }
                            let Ok(place) = usize::from_str_radix(input.trim(), 16) else {
                                continue;
                            };
                            if (place + contents.len()) <= 0xFFFF {
                                placed = true;
                                emulator.state.memory[place..place + contents.len()]
                                    .copy_from_slice(&contents);
                            }
                        }
                        execute!(
                            io::stdout(),
                            MoveTo(0, 32),
                            DisableLineWrap,
                            Clear(ClearType::FromCursorDown)
                        )?;
                        enable_raw_mode()?;
                    }
                    Err(KeyCode::F(6)) if emu_state.paused => {
                        emu_state.step = true;
                    }
                    Err(_) => {}
                }
            }
        }
        
        if emu_state.last_display.elapsed() > Duration::from_secs_f64(1.0 / 60.0) {
            emu_state.last_display = Instant::now();
            emu_state.display_reg &= 0b0111_1111;
            if let Some(old) = emu_state.old_display_buffer.take() {
                // Display screen
                for y in 0..24 {
                    for x in 0..40 {
                        if old[(y * 40 + x) as usize] == emu_state.display_buffer[(y * 40 + x) as usize]
                        {
                            continue;
                        }
                        let byte_char =
                            CHARACTER_SET[emu_state.display_buffer[(y * 40 + x) as usize] as usize];
                        execute!(io::stdout(), MoveTo(x, y), Print(byte_char as char))?;
                    }
                }

                execute!(
                    io::stdout(),
                    MoveTo(0, 30),
                    Clear(ClearType::CurrentLine),
                    Print(if emu_state.paused {
                        "F1/End: Quit   F2/Pause: Resume   F3/Home: Reset   F4/Insert: Clear   F5: Load File   F6: Step"
                    } else {
                        "F1/End: Quit   F2/Pause: Pause   F3/Home: Reset   F4/Insert: Clear   F5: Load File"
                    }),
                    MoveTo(
                        u16::from(emu_state.display_x),
                        u16::from(emu_state.display_y)
                    )
                )?;
            }
        }

        if !emu_state.paused || emu_state.step {
            if emu_state.step {
                let opcode_str =
                    Opcode::load(&emulator.state.memory[emulator.state.program_counter as usize..])
                        .unwrap_or_default()
                        .map_or("<INVALID>".into(), |op| format!("{op}"));
                execute!(
                    io::stdout(),
                    MoveTo(0, 31),
                    Clear(ClearType::CurrentLine),
                    Print(&opcode_str),
                    MoveTo(80, emu_state.step_counter),
                    Print(format!(
                        "{:04X} {opcode_str}",
                        emulator.state.program_counter
                    )),
                    MoveTo(0, 32),
                    Clear(ClearType::CurrentLine),
                    Print(format!("{:04X?}", emulator.state)),
                    MoveTo(0, 33),
                    Print(format!(
                        "XAM: {:04x} ST: {:04x} HEX: {:04x} YSAV: {:02x} MODE: {:02x}",
                        u16::from_le_bytes([
                            emulator.state.memory[0x24],
                            emulator.state.memory[0x25]
                        ]),
                        u16::from_le_bytes([
                            emulator.state.memory[0x26],
                            emulator.state.memory[0x27]
                        ]),
                        u16::from_le_bytes([
                            emulator.state.memory[0x28],
                            emulator.state.memory[0x29]
                        ]),
                        emulator.state.memory[0x2A],
                        emulator.state.memory[0x2B]
                    )),
                    MoveTo(0, 34),
                    Clear(ClearType::FromCursorDown)
                )?;
                emu_state.step_counter = emu_state.step_counter.wrapping_add(1);
            }
            
            drop(emu_state);

            let Ok(interrupt_requested) = emulator.step() else {
                // !!! INVALID OPCODE!
                continue;
            };
            if interrupt_requested {
                // Jump to IRQ vector
                let vector = u16::from_le_bytes([emulator.read(0xFFFE), emulator.read(0xFFFF)]);
                emulator.state.program_counter = vector;
            };
        }
        std::thread::sleep(Duration::from_micros(1).saturating_sub(last.elapsed()));
        last = Instant::now();
    }
}

fn keycode_ascii(code: KeyCode) -> Result<u8, KeyCode> {
    use KeyCode::{BackTab, Backspace, Char, Delete, Enter, Esc, Null, Tab};

    match code {
        Enter => Ok(b'\r'),
        Tab | BackTab => Ok(b'\t'),
        Delete => Ok(0x7f),
        Backspace => Ok(b'_'),
        Char(c) => Ok(c.to_ascii_uppercase() as u8),
        Null => Ok(0),
        Esc => Ok(0x1B),
        c => Err(c),
    }
}

// 0:a9 0 aa 20 ef ff e8 8a 4c 2 0
