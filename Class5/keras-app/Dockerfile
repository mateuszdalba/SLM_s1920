FROM python:3.6
WORKDIR /app
COPY requirements.txt /app
RUN pip install -r ./requirements.txt

COPY FMNIST_Model.h5 /app
COPY app.py /app
CMD ["python", "app.py"]~
