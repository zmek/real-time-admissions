FROM rocker/tidyverse:4.0.3

RUN apt-get update && apt-get -y install cron moreutils

COPY . /ed-crowding

COPY real-time-cron /etc/cron.d/real-time-cron
 
RUN chmod 0644 /etc/cron.d/real-time-cron

RUN chmod +x /ed-crowding/run.sh

RUN crontab /etc/cron.d/real-time-cron

WORKDIR /ed-crowding/

RUN install2.r -e blastula cowplot gridExtra lemon mlr3misc mlr3fselect \
	mlr3proba mlr3learners mlr3 polynom data.table lubridate forcats \
	stringr purrr readr tidyr tibble ggplot2 dplyr DBI stats \
	graphics grDevices utils datasets methods base xgboost

WORKDIR /ed-crowding/code

CMD ["cron", "-f", "-l", "2"]
