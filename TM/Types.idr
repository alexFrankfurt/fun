module TM.Types

import TM.TM

%access export

data Move = L | R
data State = Q0 | Qa | Qr
  | Qp | Qp1

public export
data Alphabet = O | I | S | E

Show Alphabet where
  show O = "O"
  show I = "I"
  show S = "#"
  show E = "␣"

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

--fun : Input -> Output

-- TuringMachine where
--   Set = List
--   Alphabet = [Int]

-- TODO: Create data type for concrete task, finish
-- specifications of ComputationalModel and TuringMachine

-- ComputationalModel where
--   data State = q1 | q2
--   data Move = MkMove

-- data Move = L | R

-- type synonym for convenience
-- Set : Type -> Type
-- Set = List

-- | Set of possible states of automaton
-- Q : Type
-- Q = Set State

-- | Parameterised set of possible states
-- EQ : Type -> Type
-- EQ = Set

-- Alphabet : Type
-- Alphabet = Set Char

-- | Input alphabet, ␣ ∉ Σ
-- Σ : Type
-- Σ = Alphabet

-- | Tape alphabet, ␣ ∈ Γ
-- Γ : Type
-- Γ = Alphabet

-- v : Set Int
-- v = [1,2,3]

-- t : Set Int
-- t = tail v
