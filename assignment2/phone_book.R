# Name: Alvin Han
# SciNet username: tmp_ahan
# Description:
#   script prints phone book contacts filtered out by the specified province

names <- c("Alice", "John", "Mike", "Erik", "Frank", "Charlotte")
phone.numbers <- c("416-123-4567", "647-254-3647", "519-254-6534",
                   "416-864-3425","416-463-3425", "514-635-2462")
cities <- c("Toronto", "Mississauga", "Waterloo",
            "Toronto", "Oakville","Montreal")
provinces <- c("ON", "ON", "ON", "ON", "ON", "QC")
phone.book <- data.frame(names, phone.numbers, cities, provinces)
colnames(phone.book) <- c("Name", "Phone", "City", "Province")

search.province <- "ON"

province.contacts <- phone.book[phone.book$Province == "ON", ]
cat("ON contacts:\n-----------------------------------------\n")
print(province.contacts)
cat("-----------------------------------------\n")

GTA.cities <- c("Toronto", "Mississauga", "Oakville")
non.GTA <- phone.book[!(phone.book$City %in% GTA.cities), ]
num.non.GTA <- nrow(non.GTA)
cat("Total number of contacts living outside of the Greater Toronto Area is",
      num.non.GTA, "\n")
