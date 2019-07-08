transpose [] = []
transpose ([]:xs) = transpose xs
transpose xs = map head xs : transpose (map tail xs)

