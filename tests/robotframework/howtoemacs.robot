# Copyright (C) 2019 Michael Pfennings
# SPDX-License-Identifier: GPL-3.0-or-later

*** Settings ***
Suite Setup       Open Browser To Start Slide
Suite Teardown    Close Browser
Test Setup        Go To Test Slide
Resource          resource.robot

*** Variables ***
${TEST SLIDE NUMBER}   3

*** Test Cases ***

Menu Links
  Press Menu Link   Usage
  Slide Number Should Be    13
  Press Inner Menu Link   Build Presentations on Command Line
  Slide Number Should Be    15

Slide Changing
  Go To Slide    23

Next Slide Shortcut
  Go To Next Slide
  ${newslide}    Get Slide Number
  Should Not Be Equal As Integers    ${TEST SLIDE NUMBER}    ${newslide}

Previous Slide Shortcut
  Go To Previous Slide
  ${newslide}    Get Slide Number
  Should Not Be Equal As Integers    ${TEST SLIDE NUMBER}    ${newslide}

Navigation Button Up
  Go One Slide Up
  ${newslide}    Get Slide Number
  Should Not Be Equal As Integers    ${TEST SLIDE NUMBER}    ${newslide}

Navigation Button Down
  Go One Slide Down
  ${newslide}    Get Slide Number
  Should Not Be Equal As Integers    ${TEST SLIDE NUMBER}    ${newslide}

Navigation Button Left
  Go One Slide Down
  ${newslide}    Get Slide Number
  Should Not Be Equal As Integers    ${TEST SLIDE NUMBER}    ${newslide}

Navigation Button Right
  Go One Slide Down
  ${newslide}    Get Slide Number
  Should Not Be Equal As Integers    ${TEST SLIDE NUMBER}    ${newslide}

Menu Links And Slide Changing
  Press Menu Link   Usage
  Slide Number Should Be    13
  Go To Slide   23

Check Quiz
  Go To Slide Fixed    36
  Click Element   xpath://*[@id="slickQuiz-0_question0_1"]
  Click Element   xpath://*[@id="question0"]/a[3]
  Element Should Contain    //*[@id="question0"]/ul[2]/li[1]/p    Correct!
  Element Should Contain    //*[@id="question0"]/ul[2]/li[2]/p    ${EMPTY}
  Click Element   xpath://*[@id="question0"]/a[2]
  Click Element   xpath://*[@id="question1"]/ul[1]/li[2]/label
  Click Element   xpath://*[@id="question1"]/ul[1]/li[3]/label
  Click Element   xpath://*[@id="question1"]/ul[1]/li[4]/label
  Click Element   xpath://*[@id="question1"]/a[3]
  Element Should Contain    //*[@id="question1"]/ul[2]/li[2]/p    Not Quite.

Courseware View
  Go To Slide    6
  Press Keys    None    V
  Element Should Be Visible   xpath:/html/body/div[3]
  Press Keys    None    V
  Element Should Not Be Visible   xpath:/html/body/div[3]

Speaker Notes
  Go To Slide    6
  Press Keys    None    S
  Switch Window   reveal.js - Slide Notes
  Element Should Be Visible   xpath://*[@id="speaker-controls"]
