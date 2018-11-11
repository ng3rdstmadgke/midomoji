pipeline {
  agent {
    docker {
      image 'keitamido/cent7-dev'
      args '--privileged -d -h docker-dev01'
    }

  }
  stages {
    stage('build') {
      steps {
        sh '''ls -alF
sbt package'''
      }
    }
  }
}