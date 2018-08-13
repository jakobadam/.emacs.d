#!/usr/bin/env bash
curl -o output.zip --location https://gitlab.com/gnutls/gnutls/builds/artifacts/gnutls_3_6_0_1/download?job=MinGW64/DLLs
unzip output.zip
# TODO: Copy these files to the emacs/bin directory
