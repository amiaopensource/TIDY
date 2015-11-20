module Main where

import StartApp
import Task
import Effects exposing (Never)

import TIDY exposing (init, update, view)

app = StartApp.start { init = init, view = view, update = update, inputs = [] }

main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks
