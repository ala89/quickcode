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

type trace_expr = atomic_expr array
type trace_expr' = {
  dims: int array;
  mapping: ((int * int) list, atomic_expr' Set.set) Hashtbl.t
}

type input = string array
type input_eq = {
  rows: input;
  eq: (token * token Set.set) list array;
  pos: ((int * int), pos' Set.set) Hashtbl.t 
}

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

let string_match_all (r: Str.regexp) (s: string): int list =
  let n = String.length s in
  let rec aux (i: int) (l: int list): int list =
    if i <= n then
      try
        let j = Str.search_forward r s i in
        aux (j + 1) (j :: l)
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
  
let regex_to_str (r: regex) =
  Array.fold_left (fun acc tok -> acc ^ token_to_str tok) "" r

let generate_token_equivalence (s: string) =
  let rec update matches tok = function
    | [] -> [(matches, Set.singleton tok)]
    | (matches', set) :: ls ->
      if matches' = matches then (matches, Set.add tok set) :: ls
      else (matches', set) :: update matches tok ls in

  let eq = ref [] in
  List.iter (fun tok ->
    let matches = string_match_all (Str.regexp (token_to_str tok)) s in
    eq := update matches tok !eq
  ) all_classes;

  (List.map (fun st ->
    let tok = (SpecialToken st) in
    (tok, Set.singleton tok)
  ) all_special_tokens) @ (List.map (fun (_, set) ->
    let tok = Set.choose set in
    (tok, set)
  ) !eq)

let enumerate_regexes_up_to (input: input_eq) (var: int) (k: int): (int * (token array)) list =
  let s = input.rows.(var) in
  let res = ref [(k, [||])] in
  let rec aux (init: int) (i: int) (l: token list): unit =
    if i = k then res := (init, Array.of_list (List.rev l)) :: !res
    else if i < k then begin
      List.iter (fun (tok, _) ->
        if tok <> List.hd l && Str.string_match (Str.regexp (token_to_str tok)) s i then begin (* don't test twice the same token in a row, prevents looping with null matches *)
          let j = Str.match_end () in
          aux init j (tok :: l)
        end
      ) (input.eq.(var))
    end in
  List.iter (fun (tok, _) ->
    try
      let i = Str.search_forward (Str.regexp (token_to_str tok)) s 0 in
      let j = Str.match_end () in
      aux i j [tok]
    with Not_found -> ()
  ) (input.eq.(var));
  !res

let enumerate_regexes_from (input: input_eq) (var: int) (k: int): (int * (token array)) list =
  let s = input.rows.(var) in
  let n = String.length s in
  let res = ref [(k, [||])] in
  let rec aux (i: int) (l: token list): unit =
    if i <= n then begin
      List.iter (fun (tok, _) ->
        if (l = [] || tok <> List.hd l) && Str.string_match (Str.regexp (token_to_str tok)) s i then begin (* don't test twice the same token in a row, prevents looping with null matches *)
          let j = Str.match_end () in
          res := (j, Array.of_list (List.rev (tok :: l))) :: !res;
          aux j (tok :: l)
        end
      ) (input.eq.(var))
    end in
  aux k [];
  !res

let generate_regex (input: input_eq) (var: int) (r: token array): regex' =
  Array.map (fun tok -> List.assoc tok (input.eq.(var))) r

let generate_pos (input: input_eq) (var: int) (k: int): pos' Set.set =
  match Hashtbl.find_opt input.pos (var, k) with
  | Some res -> res
  | None ->
    let s = input.rows.(var) in
    let n = String.length s in
    let res = ref (Set.of_list [CPos' k; CPos' (-(n-k+1))]) in (* first pos from the end (n) is -1 *)

    let pre_regexes = enumerate_regexes_up_to input var k in
    let post_regexes = enumerate_regexes_from input var k in

    List.iter (fun (k1, r1) ->
      List.iter (fun (_, r2) ->
        let r = Array.append r1 r2 in
        let matches = string_match_all (Str.regexp (regex_to_str r)) s in
        let c = Option.get (List.find_index ((=) k1) matches) in
        let c' = List.length matches in
        res := Set.add (Pos' (generate_regex input var r1, generate_regex input var r2, Set.of_list [c; -(c'-c)])) !res (* first match from the end is -1 *)
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
      Hashtbl.add mapping [(i, j)] set
    done
  done;
  { dims = [|n|]; mapping };;

(* Intersection *)
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
  end;;

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
        Hashtbl.add mapping (k1 @ k2) f
    ) (Hashtbl.to_seq t2.mapping)
  ) (Hashtbl.to_seq t1.mapping);
  { dims; mapping }

let make_input_eq (rows: input): input_eq =
  {
    rows;
    eq = Array.map (fun s -> generate_token_equivalence s) rows;
    pos = Hashtbl.create 0
  }

let generate_program (ios: (input * string) list): trace_expr' =
  match ios with
  | first_io :: iol ->
    let t = generate_str (make_input_eq (fst first_io)) (snd first_io) in
    List.fold_left (fun acc io ->
      intersect_trace acc (generate_str (make_input_eq (fst io)) (snd io))
    ) t iol
  | _ -> failwith "Provide at least one IO example";;


(* Choose one *)
exception Path_found of atomic_expr' Set.set list

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

let edge_starts_at =
  List.for_all2 (fun i (j, _) -> i = j)

let edge_end =
  List.map snd

let choose_one_trace (t: trace_expr'): trace_expr =
  let seen = Hashtbl.create 0 in
  let dest = Array.to_list t.dims in
  let n_dim = Array.length t.dims in
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
    explore (List.init n_dim (fun _ -> 0)) [];
    failwith "Incompatible IO examples"
  with
    Path_found p ->
      Array.of_list (List.map (fun fs ->
        choose_one_atomic (Set.choose fs)
      ) p)

(* Exec *)
let exec_pos (input: input) (p: pos) (var: int): int =
  match p with
  | CPos k ->
    if k >= 0 then k
    else String.length input.(var) + k + 1
  | Pos (r1, r2, c) ->
    let r = Array.append r1 r2 in
    let matches = string_match_all (Str.regexp (regex_to_str r)) input.(var) in
    let c' =
      if c >= 0 then c
      else List.length matches + c in
    let k1 = List.nth matches c' in
    ignore (Str.string_match (Str.regexp (regex_to_str r1)) input.(var) k1);
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