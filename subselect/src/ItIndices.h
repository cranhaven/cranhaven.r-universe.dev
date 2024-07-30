#ifndef ITIND
#define ITIND

namespace extendedleaps {

enum accesstp {d,i};
template<accesstp tp> class itindex { };
template<accesstp tp> class lagindex { };


class indexbase {
	public:
		indexbase(vind n) : nele(n)			{ }
		virtual	~indexbase(void)			{  }
		virtual void	reset(vind i=0)			{ cur_ = i; }
		virtual const bool	more(void) const	{ return (cur_ < nele); }
		virtual void operator++(int)			{ cur_++; }
		virtual void operator+=(vind inc)		{ cur_ += inc; }
	protected:
		virtual const	vind	cur(void) const		{ return cur_; }
		vind	cur_;
		vind	nele;

};


template<> class itindex<d> :  public indexbase {           /* Trivial d index type   */
	public:
		itindex<d>(vind n) : indexbase(n)			{ }
		virtual const vind operator()(void) const		{ return cur(); }
		virtual const vind	operator[](vind i) const	{ return i; }
};

template<> class itindex<i> :  public indexbase {           /* Indirect index type    */
	public:
		itindex<i>(vind n,vind* il) : indexbase(n), indlist(il) { }
		itindex<i>(vind n,vector<vind>& il) : indexbase(n), indlist(&il[0]) { }
		virtual const vind	operator()(void) const		{ return indlist[cur()]; }
		virtual const vind	operator[](vind i) const	{ return indlist[i]; }
		virtual void		asglst(vind *lst)		{ indlist = lst; }
	protected:
		vind* indlist; 
};

template<> class lagindex<d> : public itindex<d>  {  /* Lagged d index - implements an index offset   */
	public:
		lagindex<d>(vind n,vind lag) : itindex<d>(n)		{ lag_ = lag; }
		void setlag(vind lag)					{ lag_ = lag; }
		virtual void	reset(void)				{ cur_ = 0; }
		virtual void	reset(vind i)				{ cur_ = i-lag_; }
		virtual const vind	operator[](vind i) const	{ return i-lag_; }
	protected:
		vind lag_;
};

template<> class lagindex<i> : public itindex<i>  {  /* Lagged i index - implements an index offset   */
	public:
		lagindex<i>(vind n,vind lag,vind* il) : itindex<i>(n,il)  { lag_ = lag; }	
		lagindex<i>(const vind n,const vind lag,vector<vind>& il) : itindex<i>(n,il)	
			{ lag_ = lag; }
		void setlag(vind lag)					{ lag_ = lag; }
		virtual void	reset(void)				{ cur_ = 0; }
		virtual void	reset(vind i)				{ cur_ = i-lag_; }
		virtual const vind	operator[](vind i) const	{ return indlist[i-lag_]; }
	protected:
		vind lag_;
};

}

#endif
