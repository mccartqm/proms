
pain_info <- data.frame(
  pain = 1:6,
  pain_desc = c("None, or ignores it",
           "Slight, occasional, no compromise in activities",
           "Mild pain, no effect on activities, rarely moderate pain with unusual activity, may take aspirin",
           "Moderate pain, tolerable but makes concessions to pain. Some limitation of ordinary activity or work. May require occasional medicine stronger than aspirin",
           "Marked pain, serious limitation of activities",
           "Totally disabled, crippled, pain in bed, bedridden"),
  pain_score = c(44, 40, 30, 20, 10, 0)
)

limp_info <- data.frame(
  limp = 1:4,
  limp_desc = c("None", "Slight", "Moderate", "Severe"),
  limp_score = c(11, 8, 5, 0)
)

support_info <- data.frame(
  support = 1:7,
  support_desc = c("None", "Cane for long walks", "Cane most of the time", "One crutch", "Two canes", "Two crutches", "Not able to walk"),
  support_score = c(11, 7, 5, 3, 2, 0, 0)
)

distance_info <- data.frame(
  distance = 1:6,
  distance_desc = c("Unlimited", "Six blocks", "Two or three blocks", "Indoors only", "Bed to chair", "Not able to walk"),
  distance_score = c(11, 8, 5, 2, 0, 0)
)

stairs_info <- data.frame(
  stairs = 1:4,
  stairs_desc = c("Normally", "Using railing", "In any manner", "Unable to do stairs"),
  stairs_score = c(4, 2, 1, 0)
)

shoes_info <- data.frame(
  shoes = 1:3,
  shoes_desc = c("With ease", "With difficulty", "Unable"),
  shoes_score = c(4, 2, 0)
)

sitting_info <- data.frame(
  sitting = 1:3,
  sitting_desc = c("Any chair one hour", "High chair half hour", "Unable to sit comfortably"),
  sitting_score = c(5, 3, 0)
)

transport_info <- data.frame(
  transport = 1:2,
  transport_desc = c("Yes", "No"),
  transport_score = c(1, 0)
)