# Implementation of Py6S, atmospheric correction and hamonization of Landsat and Sentinel 2 image collections timeseries over water pixel sampling
## Atmospheric correction
(@Dinha explicar o processo de como colocar a imagem que vc fez que eu vou colocar na pasta DOCKER para que seguindo os outros passos que voce me passou de tudo certo)

git clone https://github.com/gee-community/ee-jupyter-contrib/


cd  ../tree/master/docker/atmcorr-ee


docker build . -t atmcorr-ee

docker run -i -t -p 8888:8888 atmcorr-ee

gcloud auth login 

git clone https://github.com/samsammurphy/gee-atmcorr-S2

git clone https://github.com/larissavaladao/py6s_harmonize_sample.git

cd py6s_harmonize_sample/

mv jupyter_notebooks ../gee-atmcorr-S2

cd ../gee-atmcorr-S2/jupyter_notebooks

jupyter-notebook py6s_correction.ipynb  --ip='*' --port=8888 --allow-root
