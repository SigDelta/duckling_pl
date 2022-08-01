current_image=$(docker images | grep -P 'duckling_pl\s+latest' | sed 's/\s\+/\t/g' | cut -f 3)
current_container=$(docker ps | grep -P 'duckling_pl:latest' | sed 's/\s\+/\t/g' | cut -f 1)
echo $current_image
echo $current_container
docker build . --no-cache -t duckling_pl:latest
docker kill $current_container
docker run -d -p 8000:8000 duckling_pl:latest
docker rmi --force $current_image
sleep 5
curl -XPOST http://0.0.0.0:8000/parse --data 'locale=pl_PL&text="100 000 złotych"'

