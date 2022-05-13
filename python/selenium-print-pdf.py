#!/usr/bin/python
# -*- coding: utf-8 -*-

# SPDX-License-Identifier: GPL-3.0-or-later
# SPDX-FileCopyrightText: 2022 Jens Lechtenbörger

"""Print reveal.js presentations to PDF with Selenium.

This script supposes that Selenium is used with Docker, e.g., as service in
a GitLab CI job or locally:
- docker run -d -p 4444:4444 -v /dev/shm:/dev/shm \
  -v $PWD:/builds/oer/OS selenium/standalone-chrome
- selenium-print-pdf.py urls.txt localhost /builds/oer/OS/public/pdfs
"""

import json
import sys

from selenium import webdriver

WAIT_TIME = 60


def get_driver(host, pdfdir):
    """Return a Chrome webdriver for use in Docker.

    See StackOverflow for settings:
    https://stackoverflow.com/questions/54578876/selenium-chrome-save-as-pdf-change-download-folder
    """
    state = {
        "recentDestinations": [
            {
                "id": "Save as PDF",
                "origin": "local",
                "account": ""
            }
        ],
        "selectedDestinationId": "Save as PDF",
        "version": 2
    }

    profile = {
        "printing.print_preview_sticky_settings.appState": json.dumps(state),
        "savefile.default_directory": pdfdir
    }

    options = webdriver.ChromeOptions()
    options.add_experimental_option("prefs", profile)
    options.add_argument("--kiosk-printing")
    options.add_argument("--start-maximized")
    driver = webdriver.Remote(
        command_executor="http://" + host + ":4444/wd/hub",
        options=options
    )
    driver.implicitly_wait(WAIT_TIME)
    return driver


def navigate_print(driver, url):
    """Navigate with driver to url and print to PDF."""
    if not url.endswith("?print-pdf"):
        url += "?print-pdf"
    driver.get(url)
    # Wait for slide number, then print.
    driver.find_element_by_class_name("slide-number-pdf")
    driver.execute_script('window.print();')


def main(urlfile, host, pdfdir):
    """Main function."""
    driver = get_driver(host, pdfdir)
    with open(urlfile, "r") as urls:
        for url in urls.readlines():
            url = url.strip()
            if url.startswith("/"):
                url = "file://" + url
            navigate_print(driver, url)
    driver.close()
    driver.quit()


if __name__ == '__main__':
    HOST = "localhost"
    PDFDIR = "/pdfs/"
    if len(sys.argv) > 1:
        if len(sys.argv) > 2:
            HOST = sys.argv[2]
            if len(sys.argv) > 3:
                PDFDIR = sys.argv[3]
    else:
        print("Usage: <{0}> urlfile [host] [pdfdir]".format(sys.argv[0]))
    main(sys.argv[1], HOST, PDFDIR)
