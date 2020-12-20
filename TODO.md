# TODO

#### Tasks
* Show isomorphism between lists and Maybe-monsters
* Show isomorphism between Identity-monsters and Streams (from Stream.hs)
* Test that functions in Operations and Data.List are analogous (when functioning on Maybe-monsters and lists respectively) - in progress
* Test that functions in Operations and Stream are analogous (when functioning on Identity-monsters and Streams respectively)
* Define equivalence of monadic streams
* DONE - Get HGL (Haskell Graphics Library) installed and working to make Breakout more playable

####Table of functions from Stream.hs to generalise/try to generalise for monsters

<table>
  <tr>
    <th>Streams</th>
    <th>MonStreams</th>
	<th>Tested?</th>
  </tr>
  <tr>
    <td>head</td>
    <td>headMS</td>
	<td>Passes</td>
  </tr>
  <tr>
    <td>tail</td>
    <td>tailMS/MMS</td>
	<td>Passes</td>
  </tr>
  <tr>
    <td>++</td>
    <td>+++</td>
	<td>Passes</td>
  </tr>
  <tr>
    <td><:></td>
    <td>variations of <:</td> 
	<td>Not yet</td>
  </tr>
  <tr>
    <td>inits</td>
    <td>initsMMS</td> 
	<td>Passes</td>
  </tr>
  <tr>
    <td>tails</td>
    <td>tailsMMS</td> 
	<td>Fails</td>
  </tr>
  <tr>
    <td>map</td>
    <td>fmap</td> 
	<td>Passes</td>
  </tr>
  <tr>
    <td>intersperse</td>
    <td>intersperseMS</td> 
	<td>Passes</td>
  </tr>
  <tr>
    <td>interleave</td>
    <td>interleaveMS</td> 
	<td>Not yet</td>
  </tr>
  <tr>
    <td>scan variations</td>
    <td>scanMMS variations</td> 
	<td>Passes</td>
  </tr>
  <tr>
    <td>transpose</td>
    <td>Not possible in general on monsters, would be interesting to try</td> 
	<td>N/A</td>
  </tr>
  <tr>
    <td>iterate</td>
    <td>iterateMS</td>
	<td>Not yet</td>
  </tr>
  <tr>
    <td>repeat</td>
    <td>repeatMS</td> 
	<td>Not yet</td>
  </tr>
  <tr>
    <td>cycle</td>
    <td>cycleMS/MMS variations</td> 
	<td>Not yet</td>
  </tr>
  <tr>
    <td>unfold</td>
    <td>unfoldMS</td> 
	<td>Not yet</td>
  </tr>
  <tr>
    <td>prefix</td>
    <td>prefixMS</td> 
	<td>Not yet</td>
  </tr>
  <tr>
    <td>take</td>
    <td>takeMMS variations</td> 
	<td>Fails</td>
  </tr>
  <tr>
    <td>drop</td>
    <td>dropMMS</td> 
	<td>Passes</td>
  </tr>
  <tr>
    <td>splitAt</td>
    <td>splitAtMS variations</td> 
	<td>Fails</td>
  </tr>
  <tr>
    <td>takeWhile</td>
    <td>takeWhileMMS</td> 
	<td>Fails</td>
  </tr>
  <tr>
    <td>dropWhile</td>
    <td>dropWhileMMS</td> 
	<td>PASSES</td>
  </tr>
  <tr>
    <td>span</td>
    <td>spanMMS</td> 
	<td>Fails</td>
  </tr>
  <tr>
    <td>break</td>
    <td>breakMMS</td> 
	<td>Fails</td>
  </tr>
  <tr>
    <td>filter</td>
    <td>filterMMS</td> 
	<td>Passes</td>
  </tr>
  <tr>
    <td>partition</td>
    <td>partitionMMS</td> 
	<td>Passes</td>
  </tr>
  <tr>
    <td>group</td>
    <td>groupMMS</td> 
	<td>Fails</td>
  </tr>
  <tr>
    <td>isPrefixOf</td>
    <td>isPrefixOfMMS</td> 
	<td>Not yet</td>
  </tr>
  <tr>
    <td>!! (indexing)</td>
    <td>!!!</td> 
	<td>Passes</td>
  </tr>
  <tr>
    <td>elemIndex</td>
    <td>elemIndexMMS</td> 
	<td>Not yet</td>
  </tr>
  <tr>
    <td>elemIndicies</td>
    <td>elemIndiciesMMS</td> 
	<td>Not yet</td>
  </tr>
  <tr>
    <td>findIndex</td>
    <td>findIndexMMS</td> 
	<td>Not yet</td>
  </tr>
  <tr>
    <td>findIndicies</td>
    <td>findIndiciesMMS</td> 
	<td>Not yet</td>
  </tr>
  <tr>
    <td>zip variations</td>
    <td>zipMMS variations</td> 
	<td>Passes</td>
  </tr>
  <tr>
    <td>distribute</td>
    <td>Doesn't work unless the join operation is idempotent - otherwise will drastically change behaviour of the monster in general</td> 
	<td>N/A</td>
  </tr>
  <tr>
    <td>unzip variations</td>
    <td>unzipMMS variations</td> 
	<td>Passes</td>
  </tr>
  <tr>
    <td>words</td>
    <td>wordsMMS</td> 
	<td>Not yet</td>
  </tr>
  <tr>
    <td>unwords</td>
    <td>unwordsMMS</td> 
	<td>Not yet</td>
  </tr>
  <tr>
    <td>lines</td>
    <td>linesMMS</td> 
	<td>Not yet</td>
  </tr>
  <tr>
    <td>unlines</td>
    <td>unlinesMMS</td> 
	<td>Not yet</td>
  </tr>
  <tr>
    <td>toList</td>
    <td>toList from foldable instance</td>
	<td>Yes</td>
  </tr>
  <tr>
    <td>fromList</td>
    <td>toMonStr</td>
	<td>Yes</td>
  </tr>
</table>