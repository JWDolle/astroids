module Pipeline where
import Model
import Collision
import Updates

-- this is for a pipelin looking like this {GameState i e p c Playing}
pipeline1:: Float -> GameState -> GameState
pipeline1 secs gstate@(GameState i e p c u s b l Playing) =
    let 
        timeUpdate = gstate{elapsedTime = e + secs }
        playerUpdate = updatePlayer timeUpdate
        enemiesUpdated = updateEnemies playerUpdate
       

        collisions = checkAllEnemyColision (player enemiesUpdated) ( comets enemiesUpdated)  (scatters enemiesUpdated) ( ufos enemiesUpdated)
        collisionChecked = if collisions  
                           then handleCollision enemiesUpdated
                           else enemiesUpdated
        updatedState = collisionChecked
    in updatedState
    