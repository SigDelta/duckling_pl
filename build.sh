echo 'provide version number:'
read version_number
docker build . --no-cache -t duckling_pl:$version_number
