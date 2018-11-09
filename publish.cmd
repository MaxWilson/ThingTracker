git checkout master && pushd %~dp0\ThingTracker\src && dotnet fable yarn-build && popd && git branch -D gh-pages && git checkout -b gh-pages && (robocopy /s ThingTracker\public . || echo "Copy complete") && git add . && git commit -m "Publish" && git push origin gh-pages -f && git checkout master