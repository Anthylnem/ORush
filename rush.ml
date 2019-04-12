open Port;;
open Moves;;
open Solver;;

let ()=
  let channel = (open_in "tests/pos9.txt")  in
  let initState = input_state channel in
  close_in channel;
  let channel = (open_in "tests/pos9.txt")  in
  let soluce = solve_input channel in
  Printf.printf "***************************\nSolution = %s\n" soluce;
  let res = check_solution initState soluce in
  if res = true then print_string "Solution Trouvée ! Nice\n"
  else print_string "Solution Non Trouvée\n"
