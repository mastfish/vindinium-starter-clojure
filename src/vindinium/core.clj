(ns vindinium.core
  (:gen-class)
  (:use [slingshot.slingshot :only [try+, throw+]])
  (:use [clojure.core.match :only (match)]))

(require '[clj-http.client :as http])

(def server-url "http://vindinium.org")
(defn secretkey [] (clojure.string/trim-newline (System/getenv "VINDINIUM_SECRET_KEY")))

(defn tiles [input]
  (get (get (get input :game) :board) :tiles)
  )
(defn total_size [input]
  (get (get (get input :game) :board) :size)
  )

(defn our_hero [input]
  (get input :hero)
  )
(defn our_position [input]
  (get (our_hero input) :pos)
  )
(defn at [[x y] tiles size]
 (tiles (+ (* y size) x)))

(defn tile_at [input, [x,y]] 
  (at [x,y] (tiles input) (total_size input))
  )

(defn both_in_range[[x, y], size]
  (and (and (<= 0 x) (<= x size)) (and (<= 0 y) (<= y size)))
  )

; Does not handle diagonals, src, dest
(defn direction_to_coord [[start_x,start_y], [end_x, end_y]]
    (cond
      (< start_x end_x) "east"
      (> start_x end_x) "west"
      (< start_y end_y) "south"
      (> start_y end_y) "north"
      :else "stay")
)

(defn move_to_coord [input, [x,y]]
  (direction_to_coord (our_position input) [x,y])
  )

(defn adjacent_coords [size, [x,y]]
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

(defn tiles_around [input, [x,y]]
  (adjacent_coords (total_size input) [x,y])
  )

(defn walkable_tiles_around [input, [x,y]]
  (filter #(not= (get (tile_at input %1) :tile) :wall) (tiles_around input (our_position input)))
  )

(defn bot [input]
  "Implement this function to create your bot!"
  ; (prn (tile_at input (our_position input)))
  ; (prn (map #(tile_at input %1) (walkable_tiles_around input (our_position input))))
  ; (prn (adjacent_coords 12 [0,0]))
  ; ()
  ; (prn (total_size input))
  (prn (our_position input))
  (let [direction 
    (move_to_coord input (first (shuffle (walkable_tiles_around input (our_position input)))))
    ]
    (prn direction)
    direction
    )
  
  ; (first (shuffle ["north", "south", "east", "west", "stay"])))
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
