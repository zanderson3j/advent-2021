(ns advent-of-code.day-16)

(declare groupPackets)

(defn hexToBinary [hex]
  (cond
    (= \0 hex) "0000"
    (= \1 hex) "0001"
    (= \2 hex) "0010"
    (= \3 hex) "0011"
    (= \4 hex) "0100"
    (= \5 hex) "0101"
    (= \6 hex) "0110"
    (= \7 hex) "0111"
    (= \8 hex) "1000"
    (= \9 hex) "1001"
    (= \A hex) "1010"
    (= \B hex) "1011"
    (= \C hex) "1100"
    (= \D hex) "1101"
    (= \E hex) "1110"
    (= \F hex) "1111"))

(defn removePadding [paddedPackets packet subPacketFlag]
  (if (true? subPacketFlag)
    paddedPackets
    (let [sizeOfPacket (count packet)
          paddingAmount (- 4 (- 4 (rem sizeOfPacket 4)))
          droppedPadding (into [] (drop paddingAmount paddedPackets))]
      droppedPadding)))

(defn appendPadding [remaining packet subPacketFlag]
  (if (true? subPacketFlag)
    packet
    (let [sizeOfPacket (count packet)
          paddingAmount (if (= (- 4 (rem sizeOfPacket 4)) 4) 0 (- 4 (rem sizeOfPacket 4)))]
      (loop [paddingToAdd (into [] (take paddingAmount remaining))
             appendedPadding packet
             remainingBits (into [] (drop (count paddingToAdd) remaining))]
        (if (or (empty? paddingToAdd) (some #{\1} paddingToAdd))
          appendedPadding
          (recur (into [] (take 4 remainingBits))
                 (into [] (concat appendedPadding paddingToAdd))
                 (into [] (drop 4 remainingBits))))))))

(defn getLiteralPacket [packets subPacketFlag]
  (loop [packet (into [] (take 6 packets))
         remaining (drop 6 packets)
         continue true
         dropPadding false]
    (if (false? continue)
      packet
      (let [nextGroup (into [] (take 5 remaining))
            continueProcessing (not dropPadding)
            dropPaddingNext (if (true? dropPadding) true (= (nth nextGroup 0) \0))]
        (recur (if (true? continueProcessing)
                 (into [] (concat packet nextGroup))
                 (appendPadding remaining packet subPacketFlag))
               (if (true? continueProcessing)
                 (into [] (drop 5 remaining))
                 (removePadding remaining packet subPacketFlag))
               continueProcessing
               dropPaddingNext)))))

(defn getOperatorPacket [packets subPacketFlag]
  (let [versionAndType (into [] (take 6 packets))
        droppedVerisonAndType (into [] (drop 6 packets))
        lengthFlag (into [] (take 1 droppedVerisonAndType))
        droppedLengthFlag (into [] (drop 1 droppedVerisonAndType))
        lenthOfLengthId (if (= (first lengthFlag) \0) 15 11)
        lengthId (into [] (take lenthOfLengthId droppedLengthFlag))
        droppedLengthId (into [] (drop lenthOfLengthId droppedLengthFlag))
        length (if (= (first lengthFlag) \0)
                 (Integer/parseInt (apply str lengthId) 2)
                 (* (Integer/parseInt (apply str lengthId) 2) 11)) ;;misread instructions, not all subpackets are length 11
        subpackets (into [] (take length droppedLengthId))
        workingData (into [] (concat versionAndType lengthFlag lengthId subpackets))
        droppedSubpackets (into [] (drop length droppedLengthId))
        paddedPacket (into [] (appendPadding droppedSubpackets workingData subPacketFlag))
        padding (into [] (drop (count workingData) paddedPacket))]
    (into [] (concat versionAndType lengthFlag lengthId (groupPackets subpackets true) padding))))

(defn getNextPacket [packets subPacketFlag]
  (if (empty? packets)
    (list)
    (let [versionAndType (take 6 packets)
          version (apply str (take 3 versionAndType))
          type (apply str (drop 3 versionAndType))]
      (if (= type "100")
        (getLiteralPacket packets subPacketFlag)
        (getOperatorPacket packets subPacketFlag)))))

(defn groupPackets [packets subPacketFlag]
  (loop [nextPacket (getNextPacket packets subPacketFlag)
         remainingPackets (into [] (drop (count (flatten nextPacket)) packets))
         groupedPackets []]
    (if (empty? nextPacket)
      groupedPackets
      (let [recurNextPacket (getNextPacket remainingPackets subPacketFlag)
            recurRemainingPackets (into [] (drop (count (flatten recurNextPacket)) remainingPackets))]
        (recur recurNextPacket recurRemainingPackets (conj groupedPackets nextPacket))))))

(defn remainingPlusNested [remaining packet]
  (loop [newRemaining remaining
         [element & restOfElements] packet]
    (if (nil? element)
      newRemaining
      (recur (if (= (type element) clojure.lang.PersistentVector)
               (conj newRemaining element)
               newRemaining)
             restOfElements))))

(defn calculateVersionSum [groupedPackets]
  (loop [[packet & remaining] groupedPackets
         sum 0]
    (if (nil? packet)
      sum
      (recur (remainingPlusNested remaining packet)
             (+ sum (Integer/parseInt (apply str (take 3 packet)) 2))))))

(defn part-1
  "Day 16 Part 1"
  [input]
  (as-> input $
        (char-array $)
        (map #(hexToBinary %) $)
        (reduce str $)
        (seq (char-array $))
        (groupPackets $ false)
        (calculateVersionSum $)))

(defn part-2
  "Day 16 Part 2"
  [input]
  input)
