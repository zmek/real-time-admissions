# About this folder

This is a script to make real-time predictions

Needs as input:
- lab_orders_to_include (created by predict-admission/clean-lab-data)
- obs_to_include (created by predict-admission/clean-obs-data)
- saved features used by each learner (created by predict-admission/run-ML); note these need to be copied across manually if new models are to be used
- ML learners themselves (created by predict-admission/run-ML); ditto
- tta_prob - probability of being admitted in a certain number of hours
- poisson_means_not_yet_arrived - for time-varying arrivals

Sends an email to bed planners with the predictions


Saves
- predictions to flow schema on EMAP. The latest predictions are retrieved by a dashboard built in Superset and displayed so that bed planners can see more detail about each patient. 

The real-time deployment of the app is done on a UCLH GAE in a Docker environment. More instructions below. 

# Docker

The following instructions assume that your terminal is in the `real-time` directory.

## Building The Docker Image

Build the image by running:

```
docker build -t [image name] .
```

Note that the tag `[image name]` can be anything that you choose to identify the image. By convention we call it `ed-crowding` or similar. The dot tells where docker should look for the Dockerfile. If the Dockerfile is not in the same directory then you would have to tell it where to look. 

To check the name given to an image:

```
docker image ls | grep "ed-c"
```


## Running The Docker Container

### Development

To execute a one off run of the code then run the following Docker command:

```
docker run --rm [image name] Rscript real-time-predictions-app.R
```

### Production

Run in the background on a cron as set by the `real-time-cron` cron file:

```
docker run -d --rm --name [process name] [image name]
```

`[process name]` can be anything you choose. It helps you identify the running container when it comes time to look at the logs or stop it. Note that currently the production image-name and process-name are both ed-crowding-prod

The `.Renviron` file in `real-time/code` requires the following key value pairs:
```
UDS_HOST=[uds host name]
UDS_USER=[username]
UDS_PWD=[password]
TO_EMAILS=[comma separated list of email addresses - no spaces]
```

## Stopping The Container

Use `docker ps | grep [process name]` to determine the hash identifier of the running container. Then `docker stop [hash]` to stop the container.

## Examining The Logs

Use `docker ps | grep [process name]` to determine the hash identifier of the running container. Then `docker logs [hash]` to list the logs. Sometimes it is easier to send the output to a file using `docker logs [hash] > logs.txt`.

## TODO:
- Try to use R Markdown in constructing the email.
