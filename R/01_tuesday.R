library("usethis")
library("gitcreds")

use_git_config(user.name = "anna-sophia21", user.email = "anna-sophia.stocker@unibe.ch")
gitcreds_get()
gitcreds_set()

use_git()
usethis::use_github()


test = 20
