#! /bin/bash
cd ~
cd .ssh/
ssh -i 'shiny-key.pem' ubuntu@ec2-3-17-139-199.us-east-2.compute.amazonaws.com
cd /srv/shiny-server/shiny_covid19_aws/
git pull origin master