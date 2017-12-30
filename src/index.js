import './main.css';
import spinnerPath from './spinner.svg';
import { Main } from './Main.elm';

Main.embed(
  document.getElementById('root'),
  { seed: Math.round(Math.random() * 2147483647), spinnerPath }
);
