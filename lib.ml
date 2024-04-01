(*
  Type definitions
  - The base types of the language are named without a '
  - Extended types that represent sets of objects are named with a '
*)

type special_token =
  | Start_token
  | End_token
  | Hyphen
  | Dot
  | Semicolon
  | Colon
  | Comma
  | Backslash
  | Slash
  | LeftPar
  | RightPar
  | LeftBra
  | RightBra
  | LeftCurlyBra
  | RightCurlyBra

type char_class =
  | Digit
  | Alpha
  | LowerAlpha
  | UpperAlpha
  | AlphaNumeric
  | Whitespace
  | Any

type token =
  | ClassP of char_class
  | NotClassP of char_class
  | SpecialToken of special_token

type regex = token array
type regex' = token Set.set array

type pos =
  | CPos of int
  | Pos of regex * regex * int
type pos' =
  | CPos' of int
  | Pos' of regex' * regex' * int Set.set

type atomic_expr =
  | Substr of int * pos * pos
  | ConstStr of string
type atomic_expr' =
  | SubStr' of int * pos' Set.set * pos' Set.set
  | ConstStr' of string

(*
  A trace expression set is represented by a graph that's vertices and edges can be determined
  on the sole basis of the dims array.
  
  If e.dims = [d1; ...; dn], then:
  - vertices are any [x1; ...; xn] st. x1 ∈ ⟦1; d1⟧, ..., xn ∈ ⟦1; dn⟧
  - edges are any ([x1; ...; xn], [y1; ...; yn]) st. x1 < y1, ..., xn < yn

  Edge labels are stored in e.mapping such that edge ([x1; ...; xn], [y1; ...; yn]) is referenced
  by the key [(x1, y1); ...; (xn, yn)]. If a key does not belong to the table, the associated
  set is empty.
*)
type trace_expr = atomic_expr array
type trace_expr' = {
  dims: int array;
  mapping: ((int * int) list, atomic_expr' Set.set) Hashtbl.t
}

(*
  An input object represents an input row from the spreadsheet. Its elements are called variables.
  Upon receival, some values are precomputed and packed together as an input_eq:
  - input.rows: original input
  - input.eq: maps variables to a partition of tokens into equivalence classes and their representant for this variable,
    eg. tok1 and tok2 belong to the same class iif their set matches is the same on input.rows.(i)
  - input.pos: an initially empty table for memoizing generate_pos results since it is always identical for a
    couple (var, k)
*)
type input = string array
type input_eq = {
  rows: input;
  eq: (token * token Set.set) list array;
  pos: ((int * int), pos' Set.set) Hashtbl.t 
}

(*
  Utility globals
*)
let all_special_tokens = [
  Start_token;
  End_token;
  Hyphen;
  Dot;
  Semicolon;
  Colon;
  Comma;
  Backslash;
  Slash;
  LeftPar;
  RightPar;
  LeftBra;
  RightBra;
  LeftCurlyBra;
  RightCurlyBra
]

let all_char_classes = [
  Digit;
  Alpha;
  LowerAlpha;
  UpperAlpha;
  AlphaNumeric;
  Whitespace;
  Any
]

let all_classes =
  (List.map (fun c -> ClassP c) all_char_classes)
  @ (List.map (fun c -> NotClassP c) all_char_classes)

(*
  Utility functions
*)
let string_match_all (r: Str.regexp) (s: string): int list =
  let n = String.length s in
  let rec aux (i: int) (l: int list): int list =
    if i <= n then
      try
        let i' = Str.search_forward r s i in
        aux (i' + 1) (i' :: l)
      with Not_found -> l
    else l
  in List.rev (aux 0 [])

let string_match_all_disjoint (r: Str.regexp) (s: string): int list =
  let n = String.length s in
  let rec aux (i: int) (l: int list): int list =
    if i <= n then
      try
        let i' = Str.search_forward r s i in
        let j = Str.match_end () in
        aux (if i = j then (i + 1) else j) (i' :: l) (* epsilon does not increase j *)
      with Not_found -> l
    else l
  in List.rev (aux 0 [])

let class_p_to_str = function
  | Digit -> {|[0-9]|}
  | Alpha -> {|[a-zA-Z]|}
  | LowerAlpha -> {|[a-z]|}
  | UpperAlpha -> {|[A-Z]|}
  | AlphaNumeric -> {|[a-zA-Z0-9]|}
  | Whitespace -> {| 	|}
  | Any -> {|.|}

let not_class_p_to_str = function
  | Digit -> {|[^0-9]|}
  | Alpha -> {|[^a-zA-Z]|}
  | LowerAlpha -> {|[^a-z]|}
  | UpperAlpha -> {|[^A-Z]|}
  | AlphaNumeric -> {|[^a-zA-Z0-9]|}
  | Whitespace -> {|[^ 	]|}
  | Any -> {|(a^)|} (* this cannot match anything *)

let special_token_to_str = function
  | Start_token -> {|^|}
  | End_token -> {|$|}
  | Hyphen -> {|-|}
  | Dot -> {|\.|}
  | Semicolon -> {|;|}
  | Colon -> {|:|}
  | Comma -> {|,|}
  | Backslash -> {|\|}
  | Slash -> {|/|}
  | LeftPar -> {|(|}
  | RightPar -> {|)|}
  | LeftBra -> {|\[|}
  | RightBra -> {|\]|}
  | LeftCurlyBra -> {|{|}
  | RightCurlyBra -> {|}|}

let token_to_str = function
  | ClassP c -> class_p_to_str c ^ "+"
  | NotClassP c -> not_class_p_to_str c ^ "+"
  | SpecialToken st -> special_token_to_str st

(* Turns a regex into a valid OCaml regexp string *)
let regex_to_caml_regexp (r: regex) =
  Str.regexp (Array.fold_left (fun acc tok -> acc ^ token_to_str tok) "" r)

let generate_token_equivalence (s: string) =
  let rec update matches tok = function
    | [] -> [(matches, Set.singleton tok)]
    | (matches', set) :: ls ->
      if matches' = matches then (matches, Set.add tok set) :: ls
      else (matches', set) :: update matches tok ls in

  let eq = ref [] in
  List.iter (fun tok ->
    let matches = string_match_all (Str.regexp (token_to_str tok)) s in
    if matches <> [] then eq := update matches tok !eq (* don't include tokens with no matches as they will never be used *)
  ) all_classes;

  (List.filter_map (fun st -> (* special tokens are computed separately as their matches are always disjoint, unless they don't match at all *)
    let tok = (SpecialToken st) in
    let matches = string_match_all (Str.regexp (token_to_str tok)) s in
    if matches <> [] then Some (tok, Set.singleton tok)
    else None
  ) all_special_tokens) @ (List.map (fun (_, set) -> (Set.choose set, set)) !eq)

let str_rev s =
  let n = String.length s in
  String.init n (fun i -> s.[n - i - 1])

let regex_rev (r: regex) =
  let n = Array.length r in
  Array.init n (fun i ->
    match r.(n - i - 1) with
    | SpecialToken End_token -> SpecialToken Start_token
    | SpecialToken Start_token -> SpecialToken End_token
    | t -> t
  )

(* Generate all couples (k2, r) st. r matches s[k:k2[ *)
let enumerate_regexes_from (s: string) (tokens: token list) (k: int): (int * (token array)) list =
  let n = String.length s in
  let res = ref [(k, [||])] in
  let rec aux (i: int) (l: token list): unit =
    if i <= n then begin
      List.iter (fun tok ->
        if (l = [] || tok <> List.hd l) && Str.string_match (Str.regexp (token_to_str tok)) s i then begin (* don't test twice the same token in a row, prevents looping with null matches *)
          let j = Str.match_end () in
          res := (j, Array.of_list (List.rev (tok :: l))) :: !res;
          aux j (tok :: l)
        end
      ) tokens
    end in
  aux k [];
  !res

(* Generate all couples (k1, r) st. r matches s[k1:k]
 It's easier to reuse the previous function by reversing the string, and then carefully reversing the result again
*)
let enumerate_regexes_up_to (s: string) (tokens: token list) (k: int): (int * (token array)) list =
  let n = String.length s in
  List.filter_map (fun (k1, r) ->
    let r' = regex_rev r in
    let k1' = n - k1 in
    ignore (Str.string_match (regex_to_caml_regexp r') s k1');
    let j = Str.match_end () in
    if j = k then Some (k1', r') (* since the results were computed starting backwards at k-1, we need to ensure that when matching forward, the match ends at k *)
    else None
  ) (enumerate_regexes_from (str_rev s) tokens (n - k))

(*
  Procedures described in the pseudo-code
*)
let generate_regex (input: input_eq) (var: int) (r: token array): regex' =
  Array.map (fun tok -> List.assoc tok (input.eq.(var))) r

let generate_pos (input: input_eq) (var: int) (k: int): pos' Set.set =
  match Hashtbl.find_opt input.pos (var, k) with
  | Some res -> res
  | None ->
    let s = input.rows.(var) in
    let n = String.length s in
    let res = ref (Set.of_list [CPos' k; CPos' (-(n-k+1))]) in (* first pos from the end (n) is -1 *)

    let tokens = List.map fst input.eq.(var) in
    let pre_regexes = enumerate_regexes_up_to input.rows.(var) tokens k in
    let post_regexes = enumerate_regexes_from input.rows.(var) tokens k in

    List.iter (fun (k1, r1) ->
      List.iter (fun (_, r2) ->
        let r = Array.append r1 r2 in
        let matches = string_match_all_disjoint (regex_to_caml_regexp r) s in
        match List.find_index ((=) k1) matches with (* since matches are disjoint, it's not guaranteed that k1 is actually a match *)
        | Some c ->
          let c' = List.length matches in
          res := Set.add (Pos' (generate_regex input var r1, generate_regex input var r2, Set.of_list [c; -(c'-c)])) !res (* first match from the end is -1 *)
        | _ -> ()        
      ) post_regexes
    ) pre_regexes;

    Hashtbl.add input.pos (var, k) !res;
    !res

let generate_substr (input: input_eq) (s: string): atomic_expr' Set.set =
  let n = String.length s in
  let res = ref Set.empty in
  for i = 0 to Array.length input.rows - 1 do
    List.iter (fun k ->
      let p1 = generate_pos input i k in
      let p2 = generate_pos input i (k + n) in
      res := Set.add (SubStr' (i, p1, p2)) !res
    ) (string_match_all (Str.regexp_string s) (input.rows.(i)))
  done;
  !res

let generate_str (input: input_eq) (s: string): trace_expr' =
  let n = String.length s in
  let mapping = Hashtbl.create (n * n) in

  for i = 0 to n - 1 do
    for j = i + 1 to n do
      let substr = String.sub s i (j - i) in
      let set = Set.add (ConstStr' substr) (generate_substr input substr) in
      Hashtbl.add mapping [(i, j)] set;
    done
  done;
  { dims = [|n|]; mapping };;

(* Intersection procedure *)

(* Given two sets s1 and s2, computes the set { e1 ∩ e2 | e1 ∈ s1, e2 ∈ s2 } where f is the intersection function *)
let intersect_generic_set (s1: 'a Set.set) (s2: 'a Set.set) (f: 'a -> 'a -> 'a option): 'a Set.set =
  let res = ref Set.empty in
  Set.iter (fun e1 ->
    Set.iter (fun e2 ->
      match f e1 e2 with
      | Some e -> res := Set.add e !res
      | _ -> ()
    ) s2  
  ) s1;
  !res

let intersect_regex (r1: regex') (r2: regex'): regex' option =
  let n1 = Array.length r1 in
  let n2 = Array.length r2 in
  if n1 <> n2 then None
  else begin
    let rec loop i l =
      if i = n1 then Some l
      else
        let t = Set.inter r1.(i) r2.(i) in
        if Set.is_empty t then None
        else loop (i + 1) (t :: l) in
    match loop 0 [] with
    | Some l -> Some (Array.of_list (List.rev l))
    | None -> None
  end

let intersect_pos (p1: pos') (p2: pos') =
  match p1, p2 with
  | CPos' k1, CPos' k2 when k1 = k2 -> Some (CPos' k1)
  | Pos' (r1, r2, c), Pos' (r1', r2', c') ->
    begin
      match intersect_regex r1 r1', intersect_regex r2 r2', Set.inter c c' with
      | Some rr1, Some rr2, cc when not (Set.is_empty cc) -> Some (Pos' (rr1, rr2, cc))
      | _ -> None
    end
  | _ -> None

let intersect_atomic (f1: atomic_expr') (f2: atomic_expr') =
  match f1, f2 with
  | ConstStr' s1, ConstStr' s2 when s1 = s2 -> Some (ConstStr' s1)
  | SubStr' (v, ps1, ps2), SubStr' (v', ps1', ps2') when v = v' ->
    begin
      match intersect_generic_set ps1 ps1' intersect_pos, intersect_generic_set ps2 ps2' intersect_pos with
      | pps1, pps2 when not (Set.is_empty pps1) && not (Set.is_empty pps2) -> Some (SubStr' (v, pps1, pps2))
      | _ -> None
    end
  | _ -> None

let intersect_trace (t1: trace_expr') (t2: trace_expr') =
  let dims = Array.append t1.dims t2.dims in
  let mapping = Hashtbl.create 0 in
  Seq.iter (fun (k1, fs1) ->
    Seq.iter (fun (k2, fs2) ->
      let f = intersect_generic_set fs1 fs2 intersect_atomic in
      if not (Set.is_empty f) then
        Hashtbl.add mapping (k1 @ k2) f;
    ) (Hashtbl.to_seq t2.mapping)
  ) (Hashtbl.to_seq t1.mapping);

  { dims; mapping }

(*
  Size
  Retrieves the size of a trace expression set  
*)
let edge_starts_at =
  List.for_all2 (fun i (j, _) -> i = j)

let edge_end =
  List.map snd

let size_regex (r: regex'): int =
  Array.fold_left (fun acc set -> Set.cardinal set * acc) 1 r

let size_pos (p: pos'): int =
  match p with
  | CPos' _ -> 1
  | Pos' (r1, r2, c) -> size_regex r1 * size_regex r2 * Set.cardinal c

let size_atomic (f: atomic_expr'): int =
  match f with
  | ConstStr' _ -> 1
  | SubStr' (v, ps1, ps2) ->
    let sum1 = Set.fold (fun p acc -> acc + size_pos p) ps1 0 in
    let sum2 = Set.fold (fun p acc -> acc + size_pos p) ps1 0 in
    sum1 * sum2

let size_trace (t: trace_expr'): int =
  let dp = Hashtbl.create 0 in
  let n_dim = Array.length t.dims in
  let src = List.init n_dim (fun _ -> 0) in
  let dest = Array.to_list t.dims in
  let edges = Hashtbl.to_seq t.mapping in

  let current = ref Set.empty in
  let frontier = ref (Set.singleton src) in

  let add_to_dp (x: int list) (n: int): unit =
    match Hashtbl.find_opt dp x with
    | Some n' -> Hashtbl.replace dp x (n + n')
    | None -> Hashtbl.add dp x n in

  add_to_dp src 1;

  while not (Set.is_empty !frontier) do
    current := !frontier;
    frontier := Set.empty;
    Set.iter (fun x ->
      let x_size = Hashtbl.find dp x in
      Seq.iter (fun (e, fs) ->
        if edge_starts_at x e then begin
          let y = edge_end e in
          add_to_dp y (x_size * (Set.fold (fun f acc -> acc + size_atomic f) fs 0));
          frontier := Set.add y !frontier
        end
      ) edges
    ) !current
  done;

  match Hashtbl.find_opt dp dest with
  | Some n -> n
  | None -> 0

(*
  Program generation from input/output examples
  Generates a trace expression set for each input/ouput example and attempts to intersect them
*)
let make_input_eq (rows: input): input_eq =
  {
    rows;
    eq = Array.map (fun s -> generate_token_equivalence s) rows;
    pos = Hashtbl.create 0
  }

let generate_program (ios: (input * string) list): trace_expr' =
  let programs = List.mapi (fun i io ->
    let t = generate_str (make_input_eq (fst io)) (snd io) in
    Printf.printf "Program %d Size: %d\n%!" i (size_trace t);
    t
  ) ios in

  match programs with
  | first_t :: ts ->
    let res = ref first_t in
    let ls = ref ts in
    let step = ref 1 in
    while not (List.is_empty !ls) do
      res := intersect_trace !res (List.hd !ls);
      Printf.printf "Step %d Size: %d\n%!" !step (size_trace !res);
      ls := (List.tl !ls);
      incr step
    done;
    !res
  | _ -> failwith "Provide at least one IO example"

(*
  Choose one
  Extract one trace expression from trace expression set
*)
let choose_one_regex (r: regex'): regex =
  Array.map (fun ts -> Set.choose ts) r

let choose_one_pos (p: pos'): pos =
  match p with
  | CPos' k -> CPos k
  | Pos' (r1, r2, c) -> Pos (choose_one_regex r1, choose_one_regex r2, Set.choose c)

let choose_one_atomic (f: atomic_expr'): atomic_expr =
  match f with
  | ConstStr' s -> ConstStr s
  | SubStr' (v, ps1, ps2) -> Substr (v, choose_one_pos (Set.choose ps1), choose_one_pos (Set.choose ps2))

exception Path_found of atomic_expr' Set.set list

let choose_one_trace (t: trace_expr'): trace_expr =
  let seen = Hashtbl.create 0 in
  let n_dim = Array.length t.dims in
  let src = List.init n_dim (fun _ -> 0) in
  let dest = Array.to_list t.dims in
  let edges = Hashtbl.to_seq t.mapping in
  
  let rec explore (x: int list) (l: atomic_expr' Set.set list) =
    if x = dest then raise (Path_found (List.rev l))
    else if not (Hashtbl.mem seen x) then begin
      Hashtbl.add seen x true;
      Seq.iter (fun (e, fs) ->
        if edge_starts_at x e then explore (edge_end e) (fs :: l)
      ) edges
    end in

  try
    explore src [];
    failwith "Incompatible IO examples"
  with
    Path_found p ->
      Array.of_list (List.map (fun fs ->
        choose_one_atomic (Set.choose fs)
      ) p)

(*
  Exec
  Run a trace expression on a given input
*)
let exec_pos (input: input) (p: pos) (var: int): int =
  match p with
  | CPos k ->
    if k >= 0 then k
    else String.length input.(var) + k + 1
  | Pos (r1, r2, c) ->
    let r = Array.append r1 r2 in
    let matches = string_match_all_disjoint (regex_to_caml_regexp r) input.(var) in
    let c' =
      if c >= 0 then c
      else List.length matches + c in
    let k1 = List.nth matches c' in
    ignore (Str.string_match (regex_to_caml_regexp r1) input.(var) k1);
    Str.match_end ()

let exec_atomic (input: input) (f: atomic_expr): string =
  match f with
  | ConstStr s' -> s'
  | Substr (v, p1, p2) ->
    let i = exec_pos input p1 v in
    let j = exec_pos input p2 v in
    String.sub input.(v) i (j - i)

let exec_trace (input: input) (t: trace_expr): string =
  Array.fold_left (fun acc f -> acc ^ (exec_atomic input f)) "" t