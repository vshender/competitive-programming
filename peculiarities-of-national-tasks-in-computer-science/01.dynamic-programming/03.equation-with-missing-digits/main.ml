let num_to_lst x = x |> String.to_seq |> List.of_seq |> List.rev

let lst_to_num x = x |> List.to_seq |> String.of_seq

let int_of_digit d = Char.code d - Char.code '0'

let digit_of_int i = Char.chr (i + Char.code '0')

let solve x y z =
  let lx = String.length x and ly = String.length y and lz = String.length z in
  let l = max (max lx ly) lz in
  let x = String.make (l - lx) '0' ^ x
  and y = String.make (l - ly) '0' ^ y
  and z = String.make (l - lz) '0' ^ z in
  let x = num_to_lst x and y = num_to_lst y and z = num_to_lst z in
  let l = List.length x in

  let dp = Array.make_matrix (l + 1) 2 None in
  dp.(0).(0) <- Some ([], [], []);

  let rec iter i x y z =
    if i > l then
      dp.(l).(0) |> Option.map (fun (x, y, z) -> lst_to_num x, lst_to_num y, lst_to_num z)
    else
      let dx = List.hd x and dy = List.hd y and dz = List.hd z in
      (match dx, dy, dz with
       | '?', '?', '?' ->
         begin
           (match dp.(i - 1).(1) with
            | Some (x', y', z') ->
              dp.(i).(0) <- Some ('0' :: x', '0' :: y', '1' :: z');
              dp.(i).(1) <- Some ('0' :: x', '9' :: y', '0' :: z')
            | None -> ());
           (match dp.(i - 1).(0) with
            | Some (x', y', z') ->
              dp.(i).(0) <- Some ('0' :: x', '0' :: y', '0' :: z');
              dp.(i).(1) <- Some ('1' :: x', '9' :: y', '0' :: z')
            | None -> ())
         end
       | '0'..'9', '?', '?' ->
         begin
           let ix = int_of_digit dx in
           (match dp.(i - 1).(1) with
            | Some (x', y', z') ->
              if ix < 9 then
                dp.(i).(0) <- Some (dx :: x', '0' :: y', digit_of_int (ix + 1) :: z');
              dp.(i).(1) <- Some (dx :: x', digit_of_int (9 - ix) :: y', '0' :: z')
            | None -> ());
           (match dp.(i - 1).(0) with
            | Some (x', y', z') ->
              dp.(i).(0) <- Some (dx :: x', '0' :: y', dx :: z');
              if ix > 0 then
                dp.(i).(1) <- Some (dx :: x', digit_of_int (10 - ix) :: y', '0' :: z')
            | None -> ())
         end
       | '?', '0'..'9', '?' ->
         begin
           let iy = int_of_digit dy in
           (match dp.(i - 1).(1) with
            | Some (x', y', z') ->
              if iy < 9 then
                dp.(i).(0) <- Some ('0' :: x', dy :: y', digit_of_int (iy + 1) :: z');
              dp.(i).(1) <- Some (digit_of_int (9 - iy) :: x', dy :: y', '0' :: z')
            | None -> ());
           (match dp.(i - 1).(0) with
            | Some (x', y', z') ->
              dp.(i).(0) <- Some ('0' :: x', dy :: y', dy :: z');
              if iy > 0 then
                dp.(i).(1) <- Some (digit_of_int (10 - iy) :: x', dy :: y', '0' :: z')
            | None -> ())
         end
       | '?', '?', '0'..'9' ->
         begin
           let iz = int_of_digit dz in
           (match dp.(i - 1).(1) with
            | Some (x', y', z') ->
              if iz > 0 then
                dp.(i).(0) <- Some (digit_of_int (iz - 1) :: x', '0' :: y', dz :: z');
              dp.(i).(1) <- Some ('9' :: x', dz :: y', dz :: z')
            | None -> ());
           (match dp.(i - 1).(0) with
            | Some (x', y', z') ->
              dp.(i).(0) <- Some (dz :: x', '0' :: y', dz :: z');
              if iz + 1 < 10 then
                dp.(i).(1) <- Some ('9' :: x', digit_of_int (iz + 1) :: y', dz :: z')
            | None -> ())
         end
       | '0'..'9', '0'..'9', '?' ->
         begin
           let ix = int_of_digit dx and iy = int_of_digit dy in
           match dp.(i - 1).(0), dp.(i - 1).(1) with
           | Some (x', y', z'), _ ->
             let sum = ix + iy in
             dp.(i).(sum / 10) <- Some (dx :: x', dy :: y', digit_of_int (sum mod 10) :: z')
           | _, Some (x', y', z') ->
             let sum = ix + iy + 1 in
             dp.(i).(sum / 10) <- Some (dx :: x', dy :: y', digit_of_int (sum mod 10) :: z')
           | _, _ ->
             ()
         end
       | '0'..'9', '?', '0'..'9' ->
         begin
           let ix = int_of_digit dx and iz = int_of_digit dz in
           match dp.(i - 1).(0), dp.(i - 1).(1) with
           | Some (x', y', z'), _ ->
             let iy = iz - ix in
             if iy >= 0 then
               dp.(i).(0) <- Some (dx :: x', digit_of_int iy :: y', dz :: z')
             else
               dp.(i).(1) <- Some (dx :: x', digit_of_int (iy + 10) :: y', dz :: z')
           | _, Some (x', y', z') ->
             let iy = iz - ix - 1 in
             if iy >= 0 then
               dp.(i).(0) <- Some (dx :: x', digit_of_int iy :: y', dz :: z')
             else
               dp.(i).(1) <- Some (dx :: x', digit_of_int (iy + 10) :: y', dz :: z')
           | _, _ ->
             ()
         end
       | '?', '0'..'9', '0'..'9' ->
         begin
           let iy = int_of_digit dy and iz = int_of_digit dz in
           match dp.(i - 1).(0), dp.(i - 1).(1) with
           | Some (x', y', z'), _ ->
             let ix = iz - iy in
             if ix >= 0 then
               dp.(i).(0) <- Some (digit_of_int ix :: x', dy :: y', dz :: z')
             else
               dp.(i).(1) <- Some (digit_of_int (ix + 10) :: x', dy :: y', dz :: z')
           | _, Some (x', y', z') ->
             let ix = iz - iy - 1 in
             if ix >= 0 then
               dp.(i).(0) <- Some (digit_of_int ix :: x', dy :: y', dz :: z')
             else
               dp.(i).(1) <- Some (digit_of_int (ix + 10) :: x', dy :: y', dz :: z')
           | _, _ ->
             ()
         end
       | '0'..'9', '0'..'9', '0'..'9' ->
         begin
           let ix = int_of_digit dx and iy = int_of_digit dy and iz = int_of_digit dz in
           (match dp.(i - 1).(1) with
            | Some (x', y', z') ->
              let sum = ix + iy + 1 in
              if sum mod 10 = iz then
                dp.(i).(sum / 10) <- Some (dx :: x', dy :: y', dz :: z')
            | None -> ());
           (match dp.(i - 1).(0) with
            | Some (x', y', z') ->
              let sum = ix + iy in
              if sum mod 10 = iz then
                dp.(i).(sum / 10) <- Some (dx :: x', dy :: y', dz :: z')
            | None -> ())
         end
       | _, _, _ -> failwith "x");
      iter (i + 1) (List.tl x) (List.tl y) (List.tl z)
  in
  iter 1 x y z |> Option.map
    (fun (x, y, z) -> (
         String.sub x (l - lx) lx,
         String.sub y (l - ly) ly,
         String.sub z (l - lz) lz
       ))

let parse_equation equation =
  let n = String.length equation
  and i = String.index equation '+'
  and j = String.index equation '=' in
  let x = String.sub equation 0 i
  and y = String.sub equation (i + 1) (j - i - 1)
  and z = String.sub equation (j + 1) (n - j - 1)
  in (x, y, z)

let main () =
  let equation = Scanf.scanf "%s" Fun.id in
  let x, y, z = parse_equation equation in
  match solve x y z with
  | Some (x, y, z) -> Printf.printf "%s+%s=%s\n" x y z
  | None           -> Printf.printf "решения не существует\n"

let () = main ()
