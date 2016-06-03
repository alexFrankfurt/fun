module TM.Types

import TM.TM
import TM.Alphabet

%access export

data Move = L | R
data State = Q0 | Qa | Qr
  | Qp | Qp1 | Qrem

proc' : State ->
        (δ : (State, Alphabet) -> (State, Alphabet, Move)) ->
        List Alphabet -> (List Alphabet, State)
proc' Qp  δ [] = ([], Qrem)
proc' Qp  δ (S :: xs) = let (lst, s) = proc' Qp δ xs in
                            (S :: lst, s)
proc' Qp  δ (O :: xs) = let (lst, s) = proc' Qp δ xs in
                            (O :: lst, s)
proc' Qp  δ (I :: xs) = (S :: xs, Qp1)
proc' Qp1 δ [] = ([], Qrem)
proc' Qp1 δ (S :: xs) = let (lst, s) = proc' Qp δ xs in
                            (I :: lst, s)
proc' Qp1 δ (I :: xs) = let (lst, s) = proc' Qp1 δ xs in
                            (O :: lst, s)
proc' Qp1 δ (O :: xs) = let (lst, s) = proc' Qp δ xs in
                            (I :: lst, s)

initState : State
initState = Q0

TuringMachine where
  Move = with Types Move
  Q = State
  Γ = Alphabet

  δ (Q0, O) = (Qp, O, R)
  δ (Q0, I) = (Qp, I, R)
  δ (Qp, O) = (Qp, O, R)
  δ (Qp, I) = (Qp1, S, L)

  q₀ = Q0
  qₐ = Qa
  qᵣ = Qr

  process s = let l = toAlphabetList s in
    case head' l of
      Nothing => "Fail"
      Just x =>
        case (initState, x) of
          (Q0, O) => case tail' l of
                          Nothing => "Fail"
                          Just l' =>
                            case proc' Qp δ l' of
                              (list, Qrem) =>
                                case head' list of
                                  Nothing => "Fail"
                                  Just x => show @{plain} $ (O :: list) ++ [S, O, x]
                              (list, Qp1) => process $ show @{plain} $ I :: list
                              (list, Qp) =>
                                case head' list of
                                  Nothing => "Fail"
                                  Just x => show @{plain} $ (O :: list) ++ [S, O, x]
          (Q0, I) => case tail' l of
                          Nothing => "Fail"
                          Just l' =>
                            case proc' Qp δ l' of
                              (list, Qrem) =>
                                case head' list of
                                  Nothing => "Fail"
                                  Just x => show @{plain} $ (I :: list) ++ [S, I, x]
                              (list, Qp1) =>  let
                                (lst, s) = proc' Qp1 δ list in
                                  case s of
                                    Qp => case head' lst of
                                      Nothing => "Fail"
                                      Just x => show @{plain} $ (I :: lst) ++ [S, I, x]
                                    Qp1 => process $ show @{plain} $ I :: lst
                                    Qrem => case head' lst of
                                      Nothing => "Fail"
                                      Just x => show @{plain} $ (I :: lst) ++ [S, I, x]
