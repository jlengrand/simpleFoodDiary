import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import * as firebase from "firebase/app";
import "firebase/auth";
import "firebase/firestore";

const firebaseConfig = {
  apiKey: process.env.ELM_APP_API_KEY,
  authDomain: process.env.ELM_APP_AUTH_DOMAIN,
  databaseURL: process.env.ELM_APP_DATABASE_URL,
  projectId: process.env.ELM_APP_PROJECT_ID,
  storageBucket: process.env.ELM_APP_STORAGE_BUCKET,
  messagingSenderId: process.env.ELM_APP_MESSAGING_SENDER_ID,
  appId: process.env.ELM_APP_APP_ID
};

// console.log(firebaseConfig);

firebase.initializeApp(firebaseConfig);

const provider = new firebase.auth.GoogleAuthProvider();
const db = firebase.firestore();

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    startingWidth: window.innerWidth,
    startingHeight: window.innerHeight
  }
});

app.ports.signIn.subscribe(() => {
  console.log("LogIn called");
  firebase
    .auth()
    .setPersistence(firebase.auth.Auth.Persistence.SESSION)
    .then(() => {
      return firebase.auth().signInWithRedirect(provider);
    })
    // .then(result => {
    //   console.log(result)
    //   result.user.getIdToken().then(idToken => {
    //     app.ports.signInInfo.send({
    //       token: idToken,
    //       email: result.user.email,
    //       uid: result.user.uid
    //     });
    //   });
    // })
    .catch(error => {
      console.log("Impossible to sign in ", error);
      // app.ports.signInError.send({
      //   code: error.code,
      //   message: error.message
      // });
    });
});

firebase.auth().getRedirectResult().then(result => {
  console.log("REDIRECT");
  console.log(result)

  result.user.getIdToken().then(idToken => {
    app.ports.signInInfo.send({
      token: idToken,
      email: result.user.email,
      uid: result.user.uid
    });
  });
})
  .catch(error => {
    console.log("Impossible to sign in ", error);
    // app.ports.signInError.send({
    //   code: error.code,
    //   message: error.message
    // });
  });


app.ports.signOut.subscribe(() => {
  console.log("LogOut called");
  firebase.auth().signOut();
});

app.ports.saveFoodLog.subscribe(data => {
  console.log(`saving message to database : ${data}`);
  console.log(data);

  // Example ts : 1598808990479

  db.collection(`users/${data.uid}/logs`)
    .add({
      content: data
    })
    .catch(error => {
      console.log("error here ", error);
    });
});


// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
