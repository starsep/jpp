#od do: [:od | q do := od].
do

" $d "


| to |
to := 0.
(2 to: 4) inject: 1 into: [:a :co | to := to + a * co. co].
to

" 60 "

| s |
(s := Set with: #yourself) add: s; collect: [:each | each size]

" a Set(2 8) "
