import keras
import numpy as np
import flask

app = flask.Flask(__name__)
model = None

def load_model():
	global model
	model = keras.models.load_model('FMNIST_Model.h5')

def prepare_image(inp):
    image = []
    for j in range(28*28):
        image.append(ord(inp.read(1)))
    arr = np.array(image).reshape(1, 28, 28, 1)/255
    return arr

@app.route("/predict", methods=["POST"])
def predict():
	data = {"success": False}
	if flask.request.method == "POST":
		if flask.request.files.get("image"):
			image = flask.request.files["image"]
			image_arr = prepare_image(image)
			preds = model.predict(image_arr).tolist()[0]
			data["predictions"] = []	
			for (label, prob) in enumerate(preds):
				r = {"label": label, "probability": float(prob)}
				data["predictions"].append(r)	
				data["success"] = True

	return flask.jsonify(data)
if __name__ == "__main__":
	print(("* Loading Keras model and Flask starting server..."
		"please wait until server has fully started"))
	load_model()
	app.run(host='0.0.0.0',threaded=False)
