s := 0.
(1 to: 3) inject: 0 into: [:a :b |
	s := s + a * b.
	b
].
s


Set class = Set new class



x := 1.
(b := [(x := x + 1) < 3]) whileTrue: b.
x


ask := nil.
ask yourself