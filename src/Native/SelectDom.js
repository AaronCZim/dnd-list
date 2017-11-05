// I've never needed doc. Maybe elm can run outside the browser or something.
const fakeNode = { querySelector: () => null, };
const doc = (typeof document !== 'undefined') ? document : fakeNode;

// A fail msg should be {
// ctor: 'MyErrorType (defined in including elm files)',
// _0 : MyErrorType Arg0,
// _1 : MyErrorType Arg1,
// ...
// _N : MyErrorType ArgN
// }
const fail = msg =>_elm_lang$core$Native_Scheduler.fail(msg)

// unit is for returning no value (indicates success as well):
const unit = _elm_lang$core$Native_Scheduler.succeed(
		_elm_lang$core$Native_Utils.Tuple0
	)

// I think rAF is needed for renders after the first:
const rAF = typeof requestAnimationFrame !== 'undefined'
	? requestAnimationFrame : cb => cb()

const onNode = (selector, apply) =>
	_elm_lang$core$Native_Scheduler.nativeBinding( cb => {
		rAF( ()=>{
			const node = document.querySelector( selector )
			if( node === null ){
				cb( fail({
					ctor: 'NotFound',
					_0: selector,
				}))
			} else
		{
				apply( node )
				cb( unit )
			}
		})
	})

const focus = id => onNode( id,  node => node.focus() )
const select = id => onNode( id,  node =>{
	node.select();
	node.focus()
})
const  _user$project$Native_SelectDom = {
	focus: focus,
	select: select,
}
