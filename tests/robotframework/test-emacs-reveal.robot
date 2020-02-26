# Copyright (C) 2019 Michael Pfennings
# Copyright (C) 2020 Jens Lechtenbörger
# SPDX-License-Identifier: GPL-3.0-or-later

*** Settings ***
Suite Setup       Open Browser To Start Slide
Suite Teardown    Close Browser
Test Setup        Go To Test Slide
Resource          resource.robot

*** Variables ***
${NOTES SLIDE NUMBER}    5
${QUIZ SLIDE NUMBER}     6

*** Test Cases ***
Initialization
  Go To Test Slide

Menu Links
  Press Menu Link           Main Part
  Slide Number Should Be    3
  Press Inner Menu Link     A slide with speaker notes
  Slide Number Should Be    ${NOTES SLIDE NUMBER}

Slide Changing
  Go To Slide    ${NOTES SLIDE NUMBER}

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
  Go One Slide Left
  ${newslide}    Get Slide Number
  Should Not Be Equal As Integers    ${TEST SLIDE NUMBER}    ${newslide}

Navigation Button Right
  Go One Slide Right
  ${newslide}    Get Slide Number
  Should Not Be Equal As Integers    ${TEST SLIDE NUMBER}    ${newslide}

Menu Links And Slide Changing
  Press Menu Link   Main Part
  Slide Number Should Be    3
  Go To Slide   5

Check Quiz
  Go To Slide    ${QUIZ SLIDE NUMBER}
  Wait Until Element Is Visible   xpath://*[@id="slickQuiz-0_question0_1"]
  Click Element                   xpath://*[@id="slickQuiz-0_question0_1"]
  Wait Until Element Is Visible   xpath://*[@id="question0"]/a[3]
  Click Element                   xpath://*[@id="question0"]/a[3]
  Element Should Contain          //*[@id="question0"]/ul[2]/li[1]/p    Correct!
  Element Should Contain          //*[@id="question0"]/ul[2]/li[2]/p    ${EMPTY}
  Wait Until Element Is Visible   xpath://*[@id="question0"]/a[2]
  Click Element                   xpath://*[@id="question0"]/a[2]
  Wait Until Element Is Visible   xpath://*[@id="question1"]/ul[1]/li[2]/label
  Click Element                   xpath://*[@id="question1"]/ul[1]/li[2]/label
  Click Element                   xpath://*[@id="question1"]/ul[1]/li[3]/label
  Click Element                   xpath://*[@id="question1"]/ul[1]/li[4]/label
  Click Element                   xpath://*[@id="question1"]/a[3]
  Element Should Contain          //*[@id="question1"]/ul[2]/li[2]/p    Not Quite.

Courseware View
  Go To Slide    ${NOTES SLIDE NUMBER}
  Press Keys    None    v
  Element Should Be Visible   ${COURSEVIEW SELECTOR}
  Press Keys    None    v
  Element Should Not Be Visible   ${COURSEVIEW SELECTOR}

Speaker Notes
  Go To Slide    ${NOTES SLIDE NUMBER}
  Press Keys    None    s
  Switch Window    reveal.js - Slide Notes
  Element Should Be Visible   xpath://*[@id="speaker-controls"]
