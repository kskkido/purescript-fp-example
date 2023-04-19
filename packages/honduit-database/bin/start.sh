function main {
  STAGE=${1:?"Missing stage"}
  BUILD_ENV=$(cat configs/.env.${STAGE})
  export $(echo ${BUILD_ENV} | xargs)
  echo ${DATABASE_CONTAINER_NAME}
  docker exec -it ${DATABASE_CONTAINER_NAME} /bin/sh
}

main $*

