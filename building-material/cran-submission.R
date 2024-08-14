library(devtools)

release()

spell_check()

check()
check_rhub()
# This function is deprecated and defunct since rhub v2.
# Please see `?rhubv2` on transitioning to the new rhub functions.
# rhubv2 is not available on my version of R

check_win_devel()
usethis::use_news_md()
usethis::use_cran_comments()
