# Copyright (C) 2019 Michael Pfennings
# SPDX-License-Identifier: GPL-3.0-or-later

*** Settings ***
Documentation     A resource file with reusable keywords and variables.
...
Library           SeleniumLibrary

*** Variables ***
${SERVER}         https://oer.gitlab.io/emacs-reveal-howto/
${BROWSER}        Firefox
${DELAY}          1
${START SLIDE}    ${SERVER}howto.html
# Folie auf der in alle Richtungen navigiert werden kann
${TEST SLIDE}      https://oer.gitlab.io/emacs-reveal-howto/howto.html#/slide-orge5c59e5
${TEST SLIDE NUMBER}   3
${NOTES URL}      ${SERVER}reveal.js/plugin/notes/notes.html
${PRESENTATION TITLE}   How to create presentations with emacs-reveal
${SLIDE WHAT}     ${START SLIDE}#/slide-what

*** Keywords ***

Open Browser To Start Slide
  Open Browser    ${START SLIDE}
  Maximize Browser Window
  Set Selenium Speed    ${DELAY}
  First Slide Should Be Open

First Slide Should Be Open
  Title Should Be   ${PRESENTATION TITLE}

Go To Test Slide
  Go To   ${TEST SLIDE}
  Slide Number Should Be    ${TEST SLIDE NUMBER}

Go One Slide Left
  Click Button  xpath:/html/body/div[2]/aside/button[1]

Go One Slide Right
  Click Button  xpath:/html/body/div[2]/aside/button[2]

Go One Slide Up
  Click Button  xpath:/html/body/div[2]/aside/button[2]

Go One Slide Down
  Click Button  xpath:/html/body/div[2]/aside/button[4]

Slide Number Should Be
  [Arguments]   ${slidenumber}
  Element Should Contain   xpath:/html/body/div[2]/div[4]/a/span   ${slidenumber}

# Folienwechsel über eingeben der Foliennummer
# Durch xpath=//body tritt der Menüfehler nicht auf
Go To Slide Fixed
  [Arguments]   ${slidenumber}
  Press Keys    None    ${slidenumber}
  Press Keys    xpath=//body    ENTER

Go To Slide
  [Arguments]   ${slidenumber}
  Press Keys    None    ${slidenumber}
  Press Keys    None    ENTER
  Slide Number Should Be    ${slidenumber}

# Kapitelauswahl
Press Menu Link
  [Arguments]    ${menuname}
  Click Element   xpath:/html/body/div[2]/footer/div[1]/div/ul/li[.//text()="${menuname}"]/a

# Unterabschnittsauswahl
Press Inner Menu Link
  [Arguments]    ${menuname}
  Click Element  xpath:/html/body/div[2]/footer/div[2]/div/ul/ul/li[.//text()="${menuname}"]/a

Go To Next Slide
  Press Keys    None    n

Go To Previous Slide
  Press Keys    None    p

Get Slide Number
  ${slidenumber}    Get Text   xpath:/html/body/div[2]/div[4]/a/span
  [return]    ${slidenumber}
