# Setup -------------------------------------------------------------------
library(readxl)
library(data.table)
library(ggplot2)
library(patchwork)

# Info --------------------------------------------------------------------
# 30 minute incubation time for mdz and omz.
# 8 hour incubation time for efavirenz and metaprolol

# Just make one sheet work
# Get data
mdz_data <-
  read_excel(
    path = "data_raw/lcms/230628RAVERSION_Short.xlsx",
    sheet = "Hydroxymidazolam",
    skip = 4,
    col_types = c(
      "text", "skip", "skip", "skip",
      "skip", "skip", "skip", "skip",
      "numeric", "skip", "skip", "text",
      "skip", "skip", "skip", "skip"
    ),
    col_names = c("sample_name", "amount", "peak_status")
  )

# Tidy data
setDT(mdz_data)
mdz_data <- mdz_data[grepl("AS", mdz_data$sample_name)] # get samples of interest
# Check peak status'
unique(mdz_data$peak_status)
# Find any that is not NA
mdz_data[!is.na(peak_status)]


mdz_data[, sample_number := tstrsplit(sample_name, "_", keep = 2, type.convert = TRUE)]
mdz_data[order(sample_number)]

sample_info <- read_excel("data_raw/lcms/AS0013B_sample_info.xlsx")

tidy_data <- merge.data.table(mdz_data, sample_info, by = "sample_name")
# Check dt structure
str(tidy_data)
# Make treatment a factor
tidy_data <- 
  tidy_data[, 
            treatment := factor(treatment,
                                levels = c("control",
                                           "medium",
                                           "0.01 ng/ml IL-6",
                                           "0.1 ng/ml IL-6",
                                           "1 ng/ml IL-6",
                                           "10 ng/ml IL-6",
                                           "0.01 ng/ml IL-1B",
                                           "0.1 ng/mI IL-1B",
                                           "1 ng/ml IL-1B",
                                           "10 ng/mI IL-1B"
                                           ),
                                ordered = TRUE
                                )
            ]


# Calculate the mean amount for each group
tidy_data[, 
          mean_amount := mean(amount, na.rm = TRUE), 
          by = .(treatment, time_treatment, time_incubation)
          ]

# get amounts relative to control
tidy_data[treatment == "control", 
          calibrator := mean(mean_amount), 
          by = .(treatment, time_treatment, time_incubation)
          ]

mdz_05h <- tidy_data[time_incubation == 0.5]
group_IL6 <- c("control",
               "medium",
               "0.01 ng/ml IL-6",
               "0.1 ng/ml IL-6",
               "1 ng/ml IL-6",
               "10 ng/ml IL-6")

plot_IL6 <- ggplot() +
  geom_point(data = mdz_05h[treatment %in% group_IL6], 
             aes(x = treatment, y = amount, color = factor(time_treatment))
             ) +
  facet_wrap(facets = vars(factor(time_treatment)))

plot_IL6

group_IL1B <- c("control",
                "medium",
                "0.01 ng/ml IL-1B",
                "0.1 ng/mI IL-1B",
                "1 ng/ml IL-1B",
                "10 ng/mI IL-1B"
               )

plot_IL1B <- ggplot() +
  geom_point(data = mdz_05h[treatment %in% group_IL1B], 
             aes(x = treatment, y = amount, color = factor(time_treatment))
  ) +
  facet_wrap(facets = vars(factor(time_treatment)))
plot_IL1B


# Import data -------------------------------------------------------------
# Import raw data
file_path <- "data_raw/lcms/230628RAVERSION_Short.xlsx"

sheets <- excel_sheets(path = file_path)
sheets <- sheets[c(3, 7, 10)]

data <-
  lapply(sheets, function(x) {
    read_excel(
      path = file_path,
      sheet = x
    )
  })





################

data <- read_excel(
  path = "data_raw/lcms/230628RAVERSION_Short.xlsx",
)


# Import sample info file
# Create sample info file

# Do absoulte activity

# create column of relative activity

# Do graphs of relative activity
