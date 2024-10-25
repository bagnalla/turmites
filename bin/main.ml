open Graphics

type direction = North | South | East | West

let turn_right = function
  | North -> East
  | South -> West
  | East -> South
  | West -> North

let turn_left = function
  | North -> West
  | South -> East
  | East -> North
  | West -> South

let move (d : direction) (x : int) (y : int) : int * int =
  match d with
  | North -> x, y - 1
  | South -> x, y + 1
  | East -> x + 1, y
  | West -> x - 1, y

let grid = Hashtbl.create 1024
let turmite_x = ref 0
let turmite_y = ref 0
let dir = ref South
let cursor_x = ref 0
let cursor_y = ref 0
let scale = ref 1
let delay = ref 0.0001
let paused = ref true

exception StepException of string

(** Only for benchmarking. *)
let step_no_draw () =
  let c = match Hashtbl.find_opt grid (!turmite_x, !turmite_y) with
    | Some c -> c
    | None -> white in
  if c == white then begin
      Hashtbl.add grid (!turmite_x, !turmite_y) black;
      dir := turn_left !dir;
    end
  else if c == black then begin
      Hashtbl.remove grid (!turmite_x, !turmite_y);
      dir := turn_right !dir;
    end
  else
    raise (StepException "unknown color");

  let x', y' = move !dir !turmite_x !turmite_y in
  turmite_x := x';
  turmite_y := y'

(** Move the turmite one step and redraw stuff that changed. *)
let step () =
  let c = match Hashtbl.find_opt grid (!turmite_x, !turmite_y) with
    | Some c -> c
    | None -> white in
  if c == white then begin
      Hashtbl.add grid (!turmite_x, !turmite_y) black;
      set_color black;
      dir := turn_left !dir;
    end
  else if c == black then begin
      Hashtbl.remove grid (!turmite_x, !turmite_y);
      set_color white;
      dir := turn_right !dir;
    end
  else
    raise (StepException "unknown color");

  let x_bound, y_bound = size_x (), size_y () in
  let actual_x = !turmite_x * !scale + !cursor_x in
  let actual_y = !turmite_y * !scale + !cursor_y in
  if actual_x >= 0 && actual_x + !scale < x_bound &&
       actual_y >= 0 && actual_y + !scale < y_bound then
    fill_rect actual_x actual_y !scale !scale;

  let x', y' = move !dir !turmite_x !turmite_y in
  turmite_x := x';
  turmite_y := y';

  set_color red;
  let actual_x = !turmite_x * !scale + !cursor_x in
  let actual_y = !turmite_y * !scale + !cursor_y in
  if actual_x >= 0 && actual_x + !scale < x_bound &&
       actual_y >= 0 && actual_y + !scale < y_bound then
    fill_circle (actual_x + !scale / 2) (actual_y + !scale / 2) (!scale / 2)

(** Clear the canvas and redraw everything from scratch. *)
let redraw () =
  clear_graph ();
  let x_bound, y_bound = size_x (), size_y () in
  Hashtbl.iter (fun (x, y) c ->
      set_color c;
      let actual_x = x * !scale + !cursor_x in
      let actual_y = y * !scale + !cursor_y in
      if actual_x >= 0 && actual_x + !scale < x_bound &&
           actual_y >= 0 && actual_y + !scale < y_bound then
        fill_rect actual_x actual_y !scale !scale
    ) grid;

  set_color red;
  let actual_x = !turmite_x * !scale + !cursor_x in
  let actual_y = !turmite_y * !scale + !cursor_y in
  if actual_x >= 0 && actual_x + !scale < x_bound &&
       actual_y >= 0 && actual_y + !scale < y_bound then
    fill_circle (actual_x + !scale / 2) (actual_y + !scale / 2) (!scale / 2)

let () =
  open_graph "";
  cursor_x := size_x () / 2;
  cursor_y := size_y () / 2;
  let t = ref (Sys.time ()) in
  while true do
    if key_pressed () then begin
        let k = read_key () in
        print_endline @@ string_of_int @@ Char.code k;
        if k == Char.chr 32 then
          paused := not !paused
        else if k == Char.chr 27 then
          raise Exit
        else if k == '-' then begin
            scale := max 1 (!scale - 1)
          end
        else if k == '=' then begin
            scale := !scale + 1
          end;
        if k == 'w' then begin
            cursor_y := !cursor_y - !scale * 5
          end;
        if k == 'a' then begin
            cursor_x := !cursor_x + !scale * 5
          end;
        if k == 's' then begin
            cursor_y := !cursor_y + !scale * 5
          end;
        if k == 'd' then begin
            cursor_x := !cursor_x - !scale * 5
          end;
        if k == 'c' then begin
            cursor_x := size_x () / 2 - !turmite_x * !scale;
            cursor_y := size_y () / 2 - !turmite_y * !scale
          end;
        redraw ()
      end;
    if not !paused && Sys.time () -. !t > !delay then begin
        step ();
        t := Sys.time ()
      end
  done

  (* for i = 1 to 1000000000 do *)
  (*   step_no_draw (); *)
  (* done *)
  (* print_endline @@ string_of_int @@ Hashtbl.length grid *)
