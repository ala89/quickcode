Printexc.record_backtrace true;;

open Lib;;

let () =
  let a = choose_one_trace (generate_program [([|"B45AACabcdef94VX"|], "abcdef"); ([|"CN4toto88"|], "toto"); ([|"desminuscules"|], "desminuscules")]) in 
  assert (exec_trace [|"B45AACabcdef94VX"|] a = "abcdef");
  assert (exec_trace [|"CN4toto88"|] a = "toto");
  assert (exec_trace [|"test"|] a = "test");
  let b = choose_one_trace (generate_program [([|"truc/sus.js"|], "truc/"); ([|"b/c/d/eggs.html"|], "b/c/d/")]) in
  assert (exec_trace [|"truc/sus.js"|] b = "truc/");
  assert (exec_trace [|"b/c/d/eggs.html"|] b = "b/c/d/");
  assert (exec_trace [|"bonk/14A/amoma/foo.h"|] b = "bonk/14A/amoma/")