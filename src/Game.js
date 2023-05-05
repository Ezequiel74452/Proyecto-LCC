import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import { joinResult, numberToColor, smallerPow2GreaterOrEqualThan } from './util';
import MyImage from './/pow.png';

let pengine;

function Game() {

  // State
  const [grid, setGrid] = useState(null);
  const [numOfColumns, setNumOfColumns] = useState(null);
  const [score, setScore] = useState(0);
  const [path, setPath] = useState([]);
  const [waiting, setWaiting] = useState(false);
  let scsq = document.getElementById('score-square');

  useEffect(() => {
    // This is executed just once, after the first render.
    PengineClient.init(onServerReady);
  }, []);

  /**
   * Called when the server was successfully initialized
   */
  function onServerReady(instance) {
    pengine = instance;
    const queryS = 'init(Grid, NumOfColumns)';
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['Grid']);
        setNumOfColumns(response['NumOfColumns']);
      }
    });
  }

  /**
   * Called while the user is drawing a path in the grid, each time the path changes.
   */
  function onPathChange(newPath) {
    // No effect if waiting.
    if (waiting) {
      return;
    }
    setPath(newPath);
    if (scsq !== null) {
      if(newPath.length===0) {
        scsq.className = "score";
        scsq.style.backgroundColor = "white";
        scsq.textContent = score.toString();
      } else {
        scsq.className = "squareInPath";
        scsq.textContent = nextBlock(newPath);
        scsq.style.backgroundColor = numberToColor(parseInt(scsq.textContent));
      }
    }
    console.log(JSON.stringify(newPath));
  }

  function nextBlock(path) {
    let res = 0;
    for (let i = 0; i < path.length; i++) {
      res += grid[path[i][0]*numOfColumns+path[i][1]];
    }
    return smallerPow2GreaterOrEqualThan(res);
  }
  /**
   * Called when the user finished drawing a path in the grid.
   */
  function onPathDone() {
    /*
    Build Prolog query, which will be like:
    join([
          64,4,64,32,16,
          64,8,16,2,32,
          2,4,64,64,2,
          2,4,32,16,4,
          16,4,16,16,16,
          16,64,2,32,32,
          64,2,64,32,64,
          32,2,64,32,4
          ], 
          5, 
          [[2, 0], [3, 0], [4, 1], [3, 1], [2, 1], [1, 1], [1, 2], [0, 3]],
          RGrids
        ).
    */
    const gridS = JSON.stringify(grid);
    const pathS = JSON.stringify(path);
    const queryS = "join(" + gridS + "," + numOfColumns + "," + pathS + ", RGrids)";
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        setScore(score + joinResult(path, grid, numOfColumns));
        setPath([]);
        animateEffect(response['RGrids']);
      } else {
        setWaiting(false);
      }
    });
    scsq.className = "score";
    scsq.style.backgroundColor = "white";
    scsq.textContent = score.toString();
  }

  /**
   * Displays each grid of the sequence as the current grid in 1sec intervals.
   * @param {number[][]} rGrids a sequence of grids.
   */
  function animateEffect(rGrids) {
    setGrid(rGrids[0]);
    const restRGrids = rGrids.slice(1);
    if (restRGrids.length > 0) {
      setTimeout(() => {
        animateEffect(restRGrids);
      }, 1000);
    } else {
      setWaiting(false);
    }
  }

  function boosterEffect() {
    let sum = JSON.stringify(0);
    const gridS = JSON.stringify(grid);
    const queryS = "boost(" + gridS + "," + numOfColumns + "," + sum + ", RGrids)";
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        setScore(score + sum);
        animateEffect(response['RGrids']);
      } else {
        setWaiting(false);
      }
    });
  }
  if (grid === null) {
    return null;
  }
  return (
    <div className="game">
      <div className="header">
        <div id = "score-square" className="score">{score}</div>
        <div className="score"> | </div>
        <div className="score">Power-Up:</div>
        <button className="btn" onClick={boosterEffect}>
          <img src={MyImage} alt="buttonpng" border="0" height={30}/>
        </button>
      </div>
      <Board
        grid={grid}
        numOfColumns={numOfColumns}
        path={path}
        onPathChange={onPathChange}
        onDone={onPathDone}
      />
    </div>
  );
}

export default Game;