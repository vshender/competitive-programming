(** Arbitrary-precision arithmetic on natural numbers. *)
module BigNat = struct
  type t = {
    num: int array;
    len: int;
  }

  let base = 10000
  let dlen = 4

  let one = { num = [| 1 |]; len = 1 }
  let two = { num = [| 2 |]; len = 1 }

  let normalize x =
    let rec normalize_iter i =
      if i = 1 || x.num.(i - 1) > 0 then
        i
      else
        normalize_iter (i - 1)
    in { x with len = normalize_iter x.len }

  let add x y =
    let maxlen = Int.max x.len y.len in
    let num = Array.make (maxlen + 1) 0 in
    let carry = ref 0 in
    for i = 0 to maxlen do
      let xi = if i < x.len then x.num.(i) else 0
      and yi = if i < y.len then y.num.(i) else 0 in
      let sum = xi + yi + !carry in
      num.(i) <- sum mod base;
      carry := sum / base
    done;
    normalize { num; len = maxlen + 1 }

  let to_string { num; len } =
    let buf = Buffer.create (len * dlen) in

    num.(len - 1) |> string_of_int |> Buffer.add_string buf;
    for i = len - 2 downto 0 do
      let s = string_of_int num.(i) in
      for _ = 0 to dlen - (String.length s) - 1 do
        Buffer.add_char buf '0'
      done;
      Buffer.add_string buf s
    done;

    Buffer.contents buf
end

(** [solve n] returns the number of sequences of length [n]. *)
let solve n =
  let open BigNat in

  (* The recurrence relation is F[k] = F[k - 1] + F[k - 2]. *)
  let rec iter fcur fprev n =
    if n = 1 then
      fcur
    else
      iter (add fcur fprev) fcur (n - 1)
  in iter two one n

let main () =
  let n = Scanf.scanf "%d" Fun.id in
  let s = solve n in
  Printf.printf "%s\n" (BigNat.to_string s)

let () = main ()
