(* Kelly Criterion Implementation *)

(* Type for different betting strategies *)
type betting_strategy =
  | Kelly       (* Kelly criterion - optimal log utility *)
  | Constant of float  (* Constant fraction of bankroll *)
  | AllIn       (* Bet entire bankroll each time *)

(* Parameters for a betting game *)
type game_params = {
  win_prob: float;    (* Probability of winning *)
  win_payoff: float;  (* Net return on win (e.g., 1.0 means double your money) *)
  loss_payoff: float; (* Net return on loss (e.g., -1.0 means lose everything bet) *)
}

(* Calculate the Kelly fraction for a given game *)
let kelly_fraction params =
  let p = params.win_prob in
  let q = 1.0 -. p in
  let b = params.win_payoff in

  (* Kelly formula: f* = p - q/b *)
  let f = p -. (q /. b) in

  (* Ensure fraction is between 0 and 1 *)
  max 0.0 (min f 1.0)

(* Calculate the optimal bet size based on strategy *)
let optimal_bet_size strategy params bankroll =
  match strategy with
  | Kelly -> kelly_fraction params *. bankroll
  | Constant frac -> frac *. bankroll
  | AllIn -> bankroll

(* Simulate a single bet *)
let make_bet params bet_amount =
  if Random.float 1.0 < params.win_prob then
    bet_amount *. params.win_payoff  (* Win *)
  else
    bet_amount *. params.loss_payoff  (* Loss *)

(* Simulate a series of bets using a given strategy *)
let simulate_strategy strategy params initial_bankroll num_bets =
  let rec aux current_bankroll bets_remaining history =
    if bets_remaining = 0 || current_bankroll <= 0.0 then
      (current_bankroll, history)
    else
      let bet_size = optimal_bet_size strategy params current_bankroll in
      let net_gain = make_bet params bet_size in
      let new_bankroll = current_bankroll +. net_gain in
      aux new_bankroll (bets_remaining - 1) (new_bankroll :: history)
  in
  aux initial_bankroll num_bets [initial_bankroll]

(* Compare different strategies over multiple trials *)
let compare_strategies params initial_bankroll num_bets num_trials =
  let strategies = [
    Kelly;
    Constant 0.1;  (* 10% of bankroll each time *)
    Constant 0.5;  (* 50% of bankroll each time *)
    AllIn
  ] in

  Random.self_init();  (* Seed the random number generator *)

  List.map (fun strategy ->
    let trial_results = Array.init num_trials (fun _ ->
      let (final_bankroll, _) = simulate_strategy strategy params initial_bankroll num_bets in
      final_bankroll
    ) in

    let avg_final = Array.fold_left (+.) 0.0 trial_results /. float_of_int num_trials in
    let max_final = Array.fold_left max 0.0 trial_results in
    let min_final = Array.fold_left (fun acc x -> if x > 0.0 then min acc x else acc) infinity trial_results in
    let bankruptcy_rate = 
      Array.fold_left (fun count br -> if br <= 0.0001 then count + 1 else count) 0 trial_results 
      |> float_of_int |> fun x -> x /. float_of_int num_trials
    in

    (strategy, avg_final, max_final, min_final, bankruptcy_rate)
  ) strategies

(* Example usage *)
let () =
  let coin_flip_game = {
    win_prob = 0.6;      (* 60% chance to win *)
    win_payoff = 1.0;    (* Double your money on win *)
    loss_payoff = -1.0;  (* Lose your bet on loss *)
  } in

  let initial_bankroll = 1000.0 in
  let num_bets = 100 in
  let num_trials = 1000 in

  let results = compare_strategies coin_flip_game initial_bankroll num_bets num_trials in

  (* Calculate Kelly fraction for this game *)
  let kelly = kelly_fraction coin_flip_game in
  Printf.printf "Kelly Criterion suggests betting %.2f%% of bankroll each time\n" (kelly *. 100.0);

  (* Print results *)
  Printf.printf "\nStrategy Comparison (%d trials of %d bets each):\n" num_trials num_bets;
  Printf.printf "%-15s %-15s %-15s %-15s %-15s\n" "Strategy" "Avg Final $" "Max Final $" "Min Final $" "Bankruptcy %";
  List.iter (fun (strategy, avg, max_val, min_val, bankruptcy) ->
    let strategy_name = match strategy with
      | Kelly -> "Kelly"
      | Constant f -> Printf.sprintf "Constant %.1f" f
      | AllIn -> "All-In"
    in
    Printf.printf "%-15s $%-14.2f $%-14.2f $%-14.2f %.2f%%\n" 
      strategy_name avg max_val min_val (bankruptcy *. 100.0)
  ) results;

  (* Detailed simulation of a single run for each strategy for visualization *)
  Printf.printf "\nDetailed single run simulation:\n";
  List.iter (fun strategy ->
    let strategy_name = match strategy with
      | Kelly -> "Kelly"
      | Constant f -> Printf.sprintf "Constant %.1f" f
      | AllIn -> "All-In"
    in

    Random.self_init();  (* Reset random seed for fair comparison *)
    let (final_bankroll, history) = simulate_strategy strategy coin_flip_game initial_bankroll num_bets in

    Printf.printf "\n%s strategy final bankroll: $%.2f\n" strategy_name final_bankroll;

    (* Print first few and last few points for visualization *)
    let history_array = Array.of_list (List.rev history) in
    let len = Array.length history_array in
    let sample_points = min 10 len in

    Printf.printf "Bankroll history (samples): ";
    for i = 0 to sample_points - 1 do
      Printf.printf "$%.0f " history_array.(i * len / sample_points)
    done;
    Printf.printf "\n";
  ) [Kelly; Constant 0.1; Constant 0.5; AllIn]

