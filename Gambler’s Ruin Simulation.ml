

(* Function to simulate a single run of the Gambler's Ruin *)
let simulate initial_bankroll bet_size =
  let rec loop bankroll steps =
    if bankroll = 0 then steps
    else if bankroll >= initial_bankroll + bet_size then steps
    else
      let outcome = Random.int 2 in
      if outcome = 0 then
        loop (bankroll - 1) (steps + 1)
      else
        loop (bankroll + 1) (steps + 1)
  in
  loop initial_bankroll 0

(* Main function to run simulations and calculate expected survival time *)
let main () =
  Random.self_init ();
  let num_simulations = 10_000 in
  let initial_bankrolls = [10; 50; 100] in
  let bet_size = 1 in

  List.iter (fun initial_bankroll ->
    let survival_times = Array.init num_simulations (fun _ -> simulate initial_bankroll bet_size) in
    let total_survival_time = Array.fold_left (+) 0 survival_times in
    let expected_survival_time = float_of_int total_survival_time /. float_of_int num_simulations in
    Printf.printf "Expected survival time with initial bankroll %d: %.2f\n"
      initial_bankroll expected_survival_time
  ) initial_bankrolls

(* Run the main function *)
let () = main ()
