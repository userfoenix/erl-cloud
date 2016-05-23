#!/bin/bash

ORIGDIR=`pwd`
cd $ORIGDIR && rebar compile skip_deps=true && \
    erl -name 'cloud@192.168.56.102' -pa ebin deps/*/ebin deps2/*/ebin ../ deps/alog/include \
        -config cloud-app.config -s cloud_app -setcookie UneTKeY123        
