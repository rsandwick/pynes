import pstats
p = pstats.Stats("test.profile")
p.sort_stats("tottime").print_stats(10)
