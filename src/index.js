import './main.css';
import { Main } from './Main.elm';

Main.embed(document.getElementById('root'), Math.round(Math.random() * 2147483647));
