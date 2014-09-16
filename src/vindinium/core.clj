(ns vindinium.core
  (:gen-class)
  (:use [slingshot.slingshot :only [try+, throw+]])
  (:use [clojure.core.match :only (match)]))

(require '[clj-http.client :as http])
(require '[clojure.core.reducers :as r])

(def server-url "http://vindinium.org")
(defn secretkey [] (clojure.string/trim-newline (System/getenv "VINDINIUM_SECRET_KEY")))

(def test-board 
  {:game 
  {:id "uc6as52d", :turn 0, :maxTurns 4
  ; , :heroes 
  ; (
  ;   {:elo 1191, :userId "xgxjnc5e", :name "mastfish", :gold 0, :spawnPos [4 10], :pos [4 10], :crashed false, :life 100, :id 1, :mineCount 0} 
  ;   {:id 2, :name "random", :pos [1 1], :life 100, :gold 0, :mineCount 0, :spawnPos [4 17], :crashed false} 
  ;   {:id 3, :name "random", :pos [23 17], :life 100, :gold 0, :mineCount 0, :spawnPos [23 17], :crashed false} 
  ;   {:id 4, :name "random", :pos [23 10], :life 100, :gold 0, :mineCount 0, :spawnPos [23 10], :crashed false}), 
  :board {:size 5, :tiles 
    [{:tile :air} {:tile :mine} {:tile :wall} {:tile :wall} {:tile :wall} 
     {:tile :air} {:tile :mine} {:tile :wall} {:tile :wall} {:tile :wall} 
     {:tile :wall} {:tile :mine} {:tile :hero, :id 1} {:tile :wall} {:tile :wall} 
     {:tile :wall} {:tile :wall} {:tile :air} {:tile :wall} {:tile :wall} 
     {:tile :wall} {:tile :mine} {:tile :air} {:tile :wall} {:tile :wall} 
     ]}, :finished false}, :hero {:elo 1191, :userId "xgxjnc5e", :name "mastfish", :gold 0, :spawnPos [4 10], :pos [2 2], :crashed false, :life 100, :id 1, :mineCount 0}, :token "4jcl", :viewUrl "http://vindinium.org/uc6as52d", :playUrl "http://vindinium.org/api/uc6as52d/4jcl/play"}
)

(defn coords_to_index [[x, y], size]
 (+ (* y size) x)
 )

(defn both_in_range[[x, y], size]
  (and (<= 0 x) (<= x size) (<= 0 y) (<= y size)))

(defn adjacent_coords [[x,y] size]
    (let [coords
        [
          [x,(- y 1)]
          [(+ x 1),y]
          [x,(+ y 1)]
          [(- x 1),y]
        ]]
      (filter #(both_in_range, %1, size) coords)
        )
  )

(defn base-score [tile,tiles]
  ; Each tile will propogate values to all tiles at this stage
  ; This should return values for each of tiles, after being painted with value from tile
    (mapv #(1) tiles )
  )

(defn scored-tiles [tiles]
  (r/fold + (map #(base-score %1 tiles) tiles)
  ))

(defn possible-moves [tiles, position, size]
  (map #(tiles (coords_to_index %1 size)) (adjacent_coords position size ))
  )

(defn best-move [tiles, position, size]
  (possible-moves tiles, position, size
  ; ((mapv score tiles) position)
  ))

(defn bot [input]
  "Implement this function to create your bot!"
  (prn (scored-tiles (:tiles (:board (:game input)))))
  ; (prn (best-move (:tiles (:board (:game input))) (:pos (:hero input))  (:size(:board (:game input)))))
  (let [direction 
    (str "stay")
    ]
    (prn direction)
    direction
    )
  
)

; Because the (y,x) position of the server is inversed. We fix it to (x,y).
(defn fix-pos [{:keys [x y]}] [y x])

(defn fix-hero [hero]
  (-> hero
      (update-in [:pos] fix-pos)
      (update-in [:spawnPos] fix-pos)))

(defn improve-input [input]
  (-> input
      (update-in [:hero] fix-hero)
      (update-in [:game :heroes] #(map fix-hero %))
      (update-in [:game :board :tiles] vec)))

(defn parse-tile [tile]
  (match (vec tile)
         [\space \space] {:tile :air}
         [\# \#] {:tile :wall}
         [\[ \]] {:tile :tavern}
         [\$ \-] {:tile :mine}
         [\$ i] {:tile :mine :of i}
         [\@ i] {:tile :hero :id (Integer/parseInt (str i))}))

(defn parse-tiles [tiles] (map parse-tile (partition 2 (seq tiles))))

(defn parse-input [input] (update-in input [:game :board :tiles] parse-tiles))

(defn request [url, params]
  "makes a POST request and returns a parsed input"
  (try+
    (-> (http/post url {:form-params params :as :json})
        :body
        parse-input
        improve-input)
    (catch map? {:keys [status body]}
      (println (str "[" status "] " body))
      (throw+))))


(defn step [from]
  (loop [input from]
    (print ".")
    (let [next (request (:playUrl input) {:dir (bot input)})]
      (if (:finished (:game next)) (println "") (recur next)))))

(defn training [secret-key turns]
  (let [input (request (str server-url "/api/training") {:key secret-key :turns turns})]
    (println (str "Starting training game " (:viewUrl input)))
    (step input)
    (println (str "Finished training game " (:viewUrl input)))))

(defn arena [secret-key games]
  (loop [it 1]
    (let [p #(println (str "[" it "/" games "] " %))
          _ (p "Waiting for pairing...")
          input (request (str server-url "/api/arena") {:key secret-key})]
      (p (str "Starting arena game " (:viewUrl input)))
      (step input)
      (p (str "Finished arena game " (:viewUrl input)))
      (when (< it games) (recur (+ it 1))))))

(def usage
  "Usage:
   training <secret-key> <number-of-turns>
   arena <secret-key> <number-of-games")

(defn -main [& args]
  (match (vec args)
         ["training", secret-key, nb] (training secret-key nb)
         ["arena", secret-key, nb] (arena secret-key nb)
         :else (println usage)))
