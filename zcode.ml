let file = "./zork1.z5"

open Bigarray

type status_line = ScoreTurns | HoursMins
type flag1_v3 = {
  status_line:  status_line;
  split_discs: bool;
  status_line_not_available: bool;
  screen_splitting_available: bool;
  font_default: bool;
}
type flag2 = {
  transcripting: bool;
  fixed_pitch_font: bool;
  screen_redraw: bool;
  use_pictures: bool;
  use_undo: bool;
  use_mouse: bool;
  use_colours: bool;
  use_sound: bool;
  use_menu: bool;
}

let map_file file =
  let fd = Unix.openfile file [Unix.O_RDONLY] 0 in
  let st = Unix.fstat fd in
  let zcode = Array1.map_file fd Int8_unsigned c_layout false Unix.(st.st_size) in
  Unix.close fd;
  zcode

let get_word zcode index =
  let high = Array1.get zcode index in
  let low = Array1.get zcode (index + 1) in
  (high lsl 8) + low


let zcode = map_file file

let version zcode = Array1.get zcode 0

let parse_flag1 zcode =
  let f = Array1.get zcode 1 in
  { status_line = if f land 0x01 = 0 then ScoreTurns else HoursMins;
    split_discs = f land 0x02 <> 0;
    status_line_not_available = f land 0x04 <> 0;
    screen_splitting_available = f land 0x08 <> 0;
    font_default = f land 0x16 <> 0 }

let parse_flag2 zcode =
  let b1 = Array1.get zcode 0x10 in
  let b2 = Array1.get zcode 0x11 in
  {transcripting   = b1 land 0x01 <> 0;
  fixed_pitch_font = b1 land 0x02 <> 0;
  screen_redraw    = b1 land 0x04 <> 0;
  use_pictures     = b1 land 0x08 <> 0;
  use_undo         = b1 land 0x10 <> 0;
  use_mouse        = b1 land 0x20 <> 0;
  use_colours      = b1 land 0x40 <> 0;
  use_sound        = b1 land 0x80 <> 0;
  use_menu         = b2 land 0x01 <> 0}

let release zcode = get_word zcode 0x2
let high_memory_base zcode = get_word zcode 0x4
let initial_pc zcode = get_word zcode 0x6
let dictionary_location zcode = get_word zcode 0x8
let object_table_location zcode = get_word zcode 0xa
let global_val_location zcode = get_word zcode 0xc
let static_memory_base zcode = get_word zcode 0xe
let abbreviations_location zcode = get_word zcode 0x18
let file_length zcode = (get_word zcode 0x1a) * 2
let checksum zcode = get_word zcode 0x1c

let parse_zcode zcode =
  let open Printf in
  printf "Version:\t%d\n" (version zcode);
  let flag1 = parse_flag1 zcode in
  printf "Status line type:\t%s\n" (if flag1.status_line = ScoreTurns then "Score/Turn" else "Hours:Mins");
  printf "Releaase:\t%d\n" (release zcode);
  printf "Base of high memory:\t0x%x\n" (high_memory_base zcode);
  printf "Initial program counter:\t0x%x\n" (initial_pc zcode);
  printf "Location of dictionary:\t0x%x\n" (dictionary_location zcode);
  printf "Location of object table:\t0x%x\n" (object_table_location zcode);
  printf "Location of global valiables:\t0x%x\n" (global_val_location zcode);
  printf "Base of static memory:\t0x%x\n" (static_memory_base zcode);
  printf "Location of abbreviations:\t0x%x\n" (abbreviations_location zcode);
  printf "File length:\t0x%x\n" (file_length zcode);
  printf "checksum:\t0x%x\n" (checksum zcode)

let () = parse_zcode zcode
