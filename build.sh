#!/bin/bash

if $(stack build); then
  stack exec ArticleAnalysis-exe res/txts/ 3
fi

