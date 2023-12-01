(** [solve pat] calculates the number of ways a valid parenthetical expression
    can be restored from [pat]. *)
let solve pat =
  let n = String.length pat in

  (* Let c[s, i] equal the difference between the total number of opening and
     closing parentheses among the first i characters of the parenthetical
     expression s.

     F[k, c] is the number of parenthetical expressions s of length k that
     match the first k characters of the pattern and such that c[s, i] >= 0
     for all i from 1 to k - 1, and c[s, k] = c. *)
  let f = Array.make_matrix (n + 1) (n + 1) 0 in
  f.(0).(0) <- 1;

  for i = 1 to n do
    for j = 0 to i do
      if pat.[i - 1] = '?' || pat.[i - 1] = '(' then
        f.(i).(j) <- if j > 0 then f.(i - 1).(j - 1) else 0;
      if pat.[i - 1] = '?' || pat.[i - 1] = ')' then
        f.(i).(j) <- f.(i).(j) + if j < n then f.(i - 1).(j + 1) else 0
    done
  done;
  f.(n).(0)

let main () =
  let pat = Scanf.scanf "%s" Fun.id in
  Printf.printf "%d\n" (solve pat)

let () = main ()
