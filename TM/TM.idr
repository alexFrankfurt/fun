module TM.TM

%access public export

interface TuringMachine where
  data Move

  data Q

  -- Should satisfy: Σ ∈ Γ, ␣ ∈ Γ
  Γ : Type

  -- Should satisfy: ␣ ∉ Σ
  -- Σ : Type

  δ : (Q, Γ) -> (Q, Γ, Move)

  q₀ : Q
  qₐ : Q
  qᵣ : Q
