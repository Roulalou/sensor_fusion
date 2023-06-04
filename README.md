Repository of the master thesis ["Gesture Recognition by Pattern Matching using Sensor Fusion on an Internet of Things device"](thesis.pdf) by Sébastien Gios.
This repository is based on the work of a previous master thesis from Sébastien Kalbusch and Vincent Verpoten.
You can find their repository here: https://github.com/sebkm/sensor_fusion


# How to use the gesture recognition
The configuration used during the thesis is:
- Ubuntu 20.04
- Erlang 25.2.3.
- Rebar3 3.19
- Rebar3_hex 7.0.4
- Rebar3_grisp 2.4.0

It could have changed with time, you can look at the requirement in the GRiSP tutorial or ask a question on Slack. Some parts of those steps are explained in more depth in the readme of the previous thesis:
- 1st: you will need a development environment, which can be achieved by following the tutorials on the GRiSP wiki
- 2nd: get the project from the Github.
- 3rd: adapt the file `wpa_supplicant.conf` under `sensor_fusion/grisp/grisp2 common/deploy/files/` to your internet configuration.
- 4th: compile the program with: `make deploy-nav_1`, you might need to change `rebar.config` to deploy on your micro-SD location.
- 5th: plug the micro-SD in your GRiSP equipped with PMOD-NAV and link it to your computer (or you can do it remotely)
- 6th: in a terminal execute this command: `sudo picocom /dev/ttyUSB1 –baud 115200 –echo` to interact with the GRiSP.
- 7th: when the booting phase is finished, calibrate the sensor and follow the steps that are shown: `sensor_fusion:set_args(nav3)`.
- 8th: when the calibration is finished, start the sensor fusion: `sensor_fusion:launch()`.
- 9th: then start the real-time classification : `sensor_fusion:realtime_once()`.
To classify 1 gesture or the default 10-second timer for the movement, you can also set the time in seconds between the parenthesis. After the classification, you will be asked if you want to add the gesture you just perform.
- 9th bis: call `sensor_fusion:realtime()` to classify multiple gesture during the default 60 seconds. Two arguments can be set, the first on the time in seconds to stop and detect the end of a gesture, and the second on the running time of the algorithm in seconds, a negative number means indefinitely.

To clean all the learned gestures from the GRiSP you can execute: `sensor_fusion:clean_gesture()`.<br>
The learned gestures can also be manually edited in the file gesture under `src/`.


# How to improve the gesture recognition algorithm
It is also possible to directly improve the gesture recognition algorithm or functions around it. All functions are written in Erlang and located in the same folder, under `src/` in the GitHub repository. Any file can be improved and tested on the GRiSP. Instructions on how to use the GRiSP are written in Appendix A of the thesis, the readme of the Github, and in the readme of the previous master thesis. All the functions for the gesture recognition were regrouped under a few files:

- **sensor_fusion.erl**: to, among other things, start Hera and the gesture recognition in real-time or clear the learned gestures.
- **csvparser.erl**: to parse CSV file or matrix (list of lists) and obtained a requested column in a list.
- **learn.erl**: regroup all the functions used for the learning part of the algorithm.
- **classify.erl**: regroup all the functions used for the classification part of the algorithm.
- **realtime.erl**: regroup all the functions used for the real-time part of the algorithm.
- **gesture**: it is the file with the learned gesture, if not empty, the gestures in the file will be already learned when the algorithm will run in real-time on the GRiSP.
- **analyze.py**: is not in src/ but in the root. It’s to plot graphs of the CSV.