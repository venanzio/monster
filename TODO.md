# TODO

#####Table of functions from Stream.hs to generalise/try to generalise for monsters

<table>
  <tr>
    <th>Streams</th>
    <th>MonStreams</th> 
  </tr>
  <tr>
    <td>head</td>
    <td>headMS</td> 
  </tr>
  <tr>
    <td>tail</td>
    <td>tailMS/MMS</td> 
  </tr>
  <tr>
    <td><:></td>
    <td>variations of <:</td> 
  </tr>
  <tr>
    <td>inits</td>
    <td>initsMMS</td> 
  </tr>
  <tr>
    <td>tails</td>
    <td>tailsMMS</td> 
  </tr>
  <tr>
    <td>map</td>
    <td>fmap</td> 
  </tr>
  <tr>
    <td>intersperse</td>
    <td>intersperseMS</td> 
  </tr>
  <tr>
    <td>interleave</td>
    <td>interleaveMS</td> 
  </tr>
  <tr>
    <td>scan variations</td>
    <td>scanMMS variations</td> 
  </tr>
  <tr>
    <td>transpose</td>
    <td>Not possible in general on monsters, would be interesting to try</td> 
  </tr>
  <tr>
    <td>iterate</td>
    <td>iterateMS</td>
  </tr>
  <tr>
    <td>repeat</td>
    <td>repeatMS</td> 
  </tr>
  <tr>
    <td>cycle</td>
    <td>cycleMS/MMS variations</td> 
  </tr>
  <tr>
    <td>unfold</td>
    <td>unfoldMS</td> 
  </tr>
  <tr>
    <td>prefix</td>
    <td>prefixMS</td> 
  </tr>
  <tr>
    <td>take</td>
    <td>takeMMS variations</td> 
  </tr>
  <tr>
    <td>drop</td>
    <td>dropMMS</td> 
  </tr>
  <tr>
    <td>splitAt</td>
    <td>splitAtMS variations</td> 
  </tr>
  <tr>
    <td>takeWhile</td>
    <td>takeWhileMMS</td> 
  </tr>
  <tr>
    <td>dropWhile</td>
    <td>dropWhileMMS</td> 
  </tr>
  <tr>
    <td>span</td>
    <td>spanMMS</td> 
  </tr>
  <tr>
    <td>break</td>
    <td>breakMMS</td> 
  </tr>
  <tr>
    <td>filter</td>
    <td>filterMMS</td> 
  </tr>
  <tr>
    <td>partition</td>
    <td>partitionMMS</td> 
  </tr>
  <tr>
    <td>group</td>
    <td>groupMMS</td> 
  </tr>
  <tr>
    <td>isPrefixOf</td>
    <td>isPrefixOfMMS</td> 
  </tr>
  <tr>
    <td>!! (indexing)</td>
    <td>!!!</td> 
  </tr>
  <tr>
    <td>elemIndex</td>
    <td>elemIndexMMS</td> 
  </tr>
  <tr>
    <td>elemIndicies</td>
    <td>elemIndiciesMMS</td> 
  </tr>
  <tr>
    <td>findIndex</td>
    <td>findIndexMMS</td> 
  </tr>
  <tr>
    <td>findIndicies</td>
    <td>findIndiciesMMS</td> 
  </tr>
  <tr>
    <td>zip variations</td>
    <td>zipMMS variations</td> 
  </tr>
  <tr>
    <td>distribute</td>
    <td>Doesn't work unless the join operation is indempotent - otherwise will drastically change behaviour of the monster in general</td> 
  </tr>
  <tr>
    <td>unzip variations</td>
    <td>unzipMMS variations</td> 
  </tr>
  <tr>
    <td>words</td>
    <td>wordsMMS</td> 
  </tr>
  <tr>
    <td>unwords</td>
    <td>unwordsMMS</td> 
  </tr>
  <tr>
    <td>lines</td>
    <td>linesMMS</td> 
  </tr>
  <tr>
    <td>unlines</td>
    <td>unlinesMMS</td> 
  </tr>
  <tr>
    <td>toList</td>
    <td>toList from foldable instance</td>
  </tr>
  <tr>
    <td>fromList</td>
    <td>toMonStr</td> 
  </tr>
</table>