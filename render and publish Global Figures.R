quarto render DemoFluxnetFigures_DM.qmd --freeze=refresh && \
touch docs/.nojekyll && \
git add docs/DemoFluxnetFigures_DM.html docs/.nojekyll && \
git commit -m "Update DemoFluxnetFigures_DM" && \
git push
