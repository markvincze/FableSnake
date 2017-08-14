rm -rf gh-pages-temp

git clone git@github.com:markvincze/FableSnake.git --branch gh-pages gh-pages-temp

rm -rf gh-pages-temp/*

cd src

dotnet fable yarn-run build

cd ..

cp public/* gh-pages-temp

cd gh-pages-temp

git add . -A
git commit -m "Publish new version"
git push origin gh-pages

cd ..

rm -rf gh-pages-temp