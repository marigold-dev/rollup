open Node
open Crypto
open Common

let empty_block level = { level; submissions = []; commits = [] }

module Vm_tests = struct
  let test_run_submissions_no_limit () =
    let submissions = [ [ 1 ]; [ 2; 3 ]; [ 4; 5; 6 ] ] in
    let initial_state = 0 in
    let result_state, _step_count =
      Vm.run_submissions submissions initial_state
    in
    let expected_state = 1 + 2 + 3 + 4 + 5 + 6 in
    Alcotest.(check int)
      "expected state matches result state" expected_state result_state

  let test_run_submissions_with_limit () =
    let submissions = [ [ 1 ]; [ 2; 3 ]; [ 4; 5; 6 ] ] in
    let initial_state = 0 in
    let result_state, _step_count =
      Vm.run_submissions ~until_step:3 submissions initial_state
    in
    let expected_state = 1 + 2 + 3 in
    Alcotest.(check int)
      "expected state matches result state" expected_state result_state

  let test_state_hashing () =
    let submissions = [ [ 1 ]; [ 2; 3 ]; [ 4; 5; 6 ] ] in
    let initial_state = 0 in
    let result_state, _step_count =
      Vm.run_submissions submissions initial_state
    in
    let result_hash = Vm.hash_state result_state |> Crypto.BLAKE2B.to_string in
    let expected_hash = Vm.hash_state 21 |> Crypto.BLAKE2B.to_string in
    Alcotest.(check string)
      "expected hash matches result hash" expected_hash result_hash
  let tests =
    let open Alcotest in
    ( "Vm",
      [
        test_case "Run submissions without limit" `Quick
          test_run_submissions_no_limit;
        test_case "Run submissions with limit" `Quick
          test_run_submissions_with_limit;
        test_case "State hashing" `Quick test_state_hashing;
      ] )
end

module Level_data_tests = struct
  (* TODO: This module is basically just a record of maps
           with a bit of extra logic. As such there are some
           nice properties that you could express with QCheck. *)
  open State_machine.Level_data

  let assert_level_data ~level_data ~level ~expected_state ~expected_submissions
      ~expected_steps =
    let { hash; vm_state; step_count } =
      find_trusted_data_for_level level level_data
    in
    Alcotest.(check int) "states match" expected_state vm_state;
    Alcotest.(check int) "steps match" expected_steps step_count;
    let result_hash = hash |> BLAKE2B.to_string in
    let expected_hash = Vm.hash_state expected_state |> BLAKE2B.to_string in
    Alcotest.(check string) "hashes match" expected_hash result_hash;
    let submissions = find_submissions_for_level level level_data in
    Alcotest.(check (list (list int)))
      "submissions match" expected_submissions submissions;
    ()

  let assert_endorsed_comit ~level_data ~level ~expected_commit =
    let commit = find_endorsed_commit level level_data in
    assert (commit = expected_commit)

  let test_initial () =
    let level_data = initial 5 21 in
    assert_level_data ~level_data ~level:5 ~expected_state:21 ~expected_steps:0
      ~expected_submissions:[]

  let test_add_level_data () =
    let initial_level = 5 in
    let initial_state = 21 in
    let level_data = initial initial_level initial_state in
    let expected_submissions = [ [ 1; 2 ]; [ 3 ] ] in
    let expected_commit =
      Commit
        {
          commit_level = 5;
          author = "Bob Ross";
          hash = Vm.hash_state initial_state;
          step_count = 0;
        }
    in
    let block =
      {
        level = initial_level + 1;
        submissions = expected_submissions;
        commits = [ expected_commit ];
      }
    in
    let level_data = add_level_data block level_data in
    let { hash = _; _ } = find_trusted_data_for_level block.level level_data in
    let expected_state = initial_state + 6 in
    assert_level_data ~level_data ~level:block.level ~expected_state
      ~expected_steps:3 ~expected_submissions;
    assert_endorsed_comit ~level_data ~level:(block.level - 1) ~expected_commit;
    let level = block.level + 1 in
    let expected_commit =
      Commit
        {
          commit_level = 6;
          author = "Bob Ross";
          hash = Vm.hash_state expected_state;
          step_count = 0;
        }
    in
    let block = { level; submissions = []; commits = [ expected_commit ] } in
    let level_data = add_level_data block level_data in
    let { vm_state; _ } = find_trusted_data_for_level level level_data in
    assert_level_data ~level_data ~level ~expected_state:vm_state
      ~expected_steps:0 ~expected_submissions:[];
    assert_endorsed_comit ~level_data ~level:(block.level - 1) ~expected_commit

  let tests =
    let open Alcotest in
    ( "Level_data",
      [
        test_case "test initial" `Quick test_initial;
        test_case "test add_level_data" `Quick test_add_level_data;
      ] )
end

module State_machine_test = struct
  open State_machine

  let test_make_rejection_games () =
    let initial_level = 5 in
    let initial_state = 21 in
    let level_data = Level_data.initial initial_level initial_state in

    let good_commit =
      Commit
        {
          commit_level = 5;
          author = "Bob Ross";
          hash = Vm.hash_state initial_state;
          step_count = 0;
        }
    in
    let bad_commit_level = 5 in
    let bad_commit_author = "Evil Bob Ross" in
    let bad_commit : commit =
      Commit
        {
          commit_level = bad_commit_level;
          author = bad_commit_author;
          hash = BLAKE2B.hash "take all your money";
          step_count = 0;
        }
    in
    let commits = [ good_commit; bad_commit ] in
    let block = { level = initial_level + 1; submissions = []; commits } in
    let level_data = Level_data.add_level_data block level_data in
    let games = make_new_rejection_games commits level_data in
    let expected_rejection_games =
      [
        Effect.Open_rejection_game
          {
            level = bad_commit_level;
            accused_author = bad_commit_author;
            midpoint_hash = Vm.hash_state initial_state;
            endorsed_commit = good_commit;
          };
      ]
    in
    assert (games = expected_rejection_games);
    ()

  let test_scenario () =
    (* Scenario:
       - block 1, submit 1
         -> Send_commit
       - block 2, commit good hash, commit bad hash submit 2
         -> Send_commit, Open_rejection_game
    *)
    let initial_level = 0 in
    let initial_state = 0 in
    let state = Level_data.initial initial_level initial_state in
    let block = { level = 1; submissions = [ 1 ]; commits = [] } in
    let (state, effects) = transition state block in 
    assert false

  let tests =
    let open Alcotest in
    ( "State_machine",
      [ test_case "test make_rejection_games" `Quick test_make_rejection_games ]
    )
end

let () =
  Alcotest.run "tests"
    [ Vm_tests.tests; Level_data_tests.tests; State_machine_test.tests ]
