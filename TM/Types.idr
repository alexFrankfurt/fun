module TM.Types

%access export

-- data State = Q1

interface ComputationalModel where
  data State

  data Move

  Set : Type -> Type
  Set = List

  Q : Type
  Q = Set State

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
