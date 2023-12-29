--gzip :: [[a]] -> [[a]]

gzip :: [[a]] -> [[a]]
gzip l = if any null l then [] else map head l : gzip (map tail l)