// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; tab-width: 4 -*-
//
// plus.h: Rcpp R/C++ interface class library -- operator+
//
// Copyright (C) 2010 - 2011 Dirk Eddelbuettel and Romain Francois
//
// This file is part of Rcpp.
//
// Rcpp is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// Rcpp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Rcpp.  If not, see <http://www.gnu.org/licenses/>.

#ifndef Rcpp__sugar__plus_h
#define Rcpp__sugar__plus_h

namespace Rcpp{
namespace sugar{

	template <int RTYPE, bool LHS_NA, typename LHS_T, bool RHS_NA, typename RHS_T >
	class Plus_Vector_Vector : public Rcpp::VectorBase<RTYPE, true , Plus_Vector_Vector<RTYPE,LHS_NA,LHS_T,RHS_NA,RHS_T> > {
	public:
		typedef typename traits::storage_type<RTYPE>::type STORAGE ;
		typedef typename Rcpp::VectorBase<RTYPE,LHS_NA,LHS_T> LHS_TYPE ;
		typedef typename Rcpp::VectorBase<RTYPE,RHS_NA,RHS_T> RHS_TYPE ;

		typedef typename Rcpp::traits::Extractor< RTYPE, LHS_NA, LHS_T>::type LHS_EXT ;
		typedef typename Rcpp::traits::Extractor< RTYPE, RHS_NA, RHS_T>::type RHS_EXT ;

		Plus_Vector_Vector( const LHS_TYPE& lhs_, const RHS_TYPE& rhs_ ) :
			lhs(lhs_.get_ref()), rhs(rhs_.get_ref()) {}

		inline STORAGE operator[]( int i ) const {
			STORAGE lhs_ = lhs[i] ;
			if( traits::is_na<RTYPE>(lhs_) ) return lhs_ ;
			STORAGE rhs_ = rhs[i] ;
			return traits::is_na<RTYPE>(rhs_) ? rhs_ : (lhs_ + rhs_) ;
		}

		inline int size() const { return lhs.size() ; }

	private:
		const LHS_EXT& lhs ;
		const RHS_EXT& rhs ;
	} ;
	// specialization of the above for REALSXP because :
	// NA_REAL + NA_REAL = NA_REAL
	// NA_REAL + x = NA_REAL
	// x + NA_REAL = NA_REAL
	template <bool LHS_NA, typename LHS_T, bool RHS_NA, typename RHS_T >
	class Plus_Vector_Vector<REALSXP,LHS_NA,LHS_T,RHS_NA,RHS_T> :
	    public Rcpp::VectorBase<REALSXP, true , Plus_Vector_Vector<REALSXP,LHS_NA,LHS_T,RHS_NA,RHS_T> > {
	public:
		typedef typename Rcpp::VectorBase<REALSXP,LHS_NA,LHS_T> LHS_TYPE ;
		typedef typename Rcpp::VectorBase<REALSXP,RHS_NA,RHS_T> RHS_TYPE ;

		typedef typename Rcpp::traits::Extractor<REALSXP, LHS_NA, LHS_T>::type LHS_EXT ;
		typedef typename Rcpp::traits::Extractor<REALSXP, RHS_NA, RHS_T>::type RHS_EXT ;

		Plus_Vector_Vector( const LHS_TYPE& lhs_, const RHS_TYPE& rhs_ ) :
			lhs(lhs_.get_ref()), rhs(rhs_.get_ref()) {}

		inline double operator[]( int i ) const {
			return lhs[i] + rhs[i] ;
		}

		inline int size() const { return lhs.size() ; }

	private:
		const LHS_EXT& lhs ;
		const RHS_EXT& rhs ;
	} ;




	// specialization LHS_NA = false
	template <int RTYPE, typename LHS_T, bool RHS_NA, typename RHS_T >
	class Plus_Vector_Vector<RTYPE,false,LHS_T,RHS_NA,RHS_T> : public Rcpp::VectorBase<RTYPE,true, Plus_Vector_Vector<RTYPE,false,LHS_T,RHS_NA,RHS_T> > {
	public:
		typedef typename traits::storage_type<RTYPE>::type STORAGE ;
		typedef typename Rcpp::VectorBase<RTYPE,false,LHS_T> LHS_TYPE ;
		typedef typename Rcpp::VectorBase<RTYPE,RHS_NA,RHS_T> RHS_TYPE ;

		typedef typename Rcpp::traits::Extractor< RTYPE, false, LHS_T>::type LHS_EXT ;
		typedef typename Rcpp::traits::Extractor< RTYPE, RHS_NA, RHS_T>::type RHS_EXT ;

		Plus_Vector_Vector( const LHS_TYPE& lhs_, const RHS_TYPE& rhs_ ) :
			lhs(lhs_.get_ref()), rhs(rhs_.get_ref()){}

		inline STORAGE operator[]( int i ) const {
			STORAGE rhs_ = rhs[i] ;
			if( traits::is_na<RTYPE>(rhs_) ) return rhs_ ;
			return lhs[i] + rhs_  ;
		}

		inline int size() const { return lhs.size() ; }

	private:
		const LHS_EXT& lhs ;
		const RHS_EXT& rhs ;
	} ;
	// LHS_NA = false & RTYPE = REALSXP
	template <typename LHS_T, bool RHS_NA, typename RHS_T >
	class Plus_Vector_Vector<REALSXP,false,LHS_T,RHS_NA,RHS_T> :
	    public Rcpp::VectorBase<REALSXP,true, Plus_Vector_Vector<REALSXP,false,LHS_T,RHS_NA,RHS_T> > {
	public:
		typedef typename Rcpp::VectorBase<REALSXP,false,LHS_T> LHS_TYPE ;
		typedef typename Rcpp::VectorBase<REALSXP,RHS_NA,RHS_T> RHS_TYPE ;

		typedef typename Rcpp::traits::Extractor<REALSXP, false, LHS_T>::type LHS_EXT ;
		typedef typename Rcpp::traits::Extractor<REALSXP, RHS_NA, RHS_T>::type RHS_EXT ;

		Plus_Vector_Vector( const LHS_TYPE& lhs_, const RHS_TYPE& rhs_ ) :
			lhs(lhs_.get_ref()), rhs(rhs_.get_ref()){}

		inline double operator[]( int i ) const {
			return lhs[i] + rhs[i] ;
		}

		inline int size() const { return lhs.size() ; }

	private:
		const LHS_EXT& lhs ;
		const RHS_EXT& rhs ;
	} ;



	// specialization for RHS_NA = false
	template <int RTYPE, bool LHS_NA, typename LHS_T, typename RHS_T >
	class Plus_Vector_Vector<RTYPE,LHS_NA,LHS_T,false,RHS_T> : public Rcpp::VectorBase<RTYPE, true , Plus_Vector_Vector<RTYPE,LHS_NA,LHS_T,false,RHS_T> > {
	public:
		typedef typename traits::storage_type<RTYPE>::type STORAGE ;
		typedef typename Rcpp::VectorBase<RTYPE,LHS_NA,LHS_T> LHS_TYPE ;
		typedef typename Rcpp::VectorBase<RTYPE,false,RHS_T> RHS_TYPE ;

		typedef typename Rcpp::traits::Extractor< RTYPE, LHS_NA, LHS_T>::type LHS_EXT ;
		typedef typename Rcpp::traits::Extractor< RTYPE, false, RHS_T>::type RHS_EXT ;

		Plus_Vector_Vector( const LHS_TYPE& lhs_, const RHS_TYPE& rhs_ ) :
			lhs(lhs_.get_ref()), rhs(rhs_.get_ref()){}

		inline STORAGE operator[]( int i ) const {
			STORAGE lhs_ = lhs[i] ;
			if( traits::is_na<RTYPE>(lhs_) ) return lhs_ ;
			return lhs_ + rhs[i]  ;
		}

		inline int size() const { return lhs.size() ; }

	private:
		const LHS_EXT& lhs ;
		const RHS_EXT& rhs ;
	} ;
    // RHS_NA = false, RTYPE = REALSXP
	template <bool LHS_NA, typename LHS_T, typename RHS_T >
	class Plus_Vector_Vector<REALSXP,LHS_NA,LHS_T,false,RHS_T> :
	    public Rcpp::VectorBase<REALSXP, true , Plus_Vector_Vector<REALSXP,LHS_NA,LHS_T,false,RHS_T> > {
	public:
		typedef typename Rcpp::VectorBase<REALSXP,LHS_NA,LHS_T> LHS_TYPE ;
		typedef typename Rcpp::VectorBase<REALSXP,false,RHS_T> RHS_TYPE ;

		typedef typename Rcpp::traits::Extractor<REALSXP, LHS_NA, LHS_T>::type LHS_EXT ;
		typedef typename Rcpp::traits::Extractor<REALSXP, false, RHS_T>::type RHS_EXT ;

		Plus_Vector_Vector( const LHS_TYPE& lhs_, const RHS_TYPE& rhs_ ) :
			lhs(lhs_.get_ref()), rhs(rhs_.get_ref()){}

		inline double operator[]( int i ) const {
			return lhs[i] + rhs[i] ;
		}

		inline int size() const { return lhs.size() ; }

	private:
		const LHS_EXT& lhs ;
		const RHS_EXT& rhs ;
	} ;




	// specialization for RHS_NA = false  and LHS_NA = false
	template <int RTYPE, typename LHS_T, typename RHS_T >
	class Plus_Vector_Vector<RTYPE,false,LHS_T,false,RHS_T> : public Rcpp::VectorBase<RTYPE, false , Plus_Vector_Vector<RTYPE,false,LHS_T,false,RHS_T> > {
	public:
		typedef typename traits::storage_type<RTYPE>::type STORAGE ;
		typedef typename Rcpp::VectorBase<RTYPE,false,LHS_T> LHS_TYPE ;
		typedef typename Rcpp::VectorBase<RTYPE,false,RHS_T> RHS_TYPE ;

		typedef typename Rcpp::traits::Extractor< RTYPE, false, LHS_T>::type LHS_EXT ;
		typedef typename Rcpp::traits::Extractor< RTYPE, false, RHS_T>::type RHS_EXT ;

		Plus_Vector_Vector( const LHS_TYPE& lhs_, const RHS_TYPE& rhs_ ) :
			lhs(lhs_.get_ref()), rhs(rhs_.get_ref()){}

		inline STORAGE operator[]( int i ) const {
			return lhs[i] + rhs[i]  ;
		}

		inline int size() const { return lhs.size() ; }

	private:
		const LHS_EXT& lhs ;
		const RHS_EXT& rhs ;
	} ;
	// specialization for RHS_NA = false  and LHS_NA = false, RTYPE = REALSXP
	template <typename LHS_T, typename RHS_T >
	class Plus_Vector_Vector<REALSXP,false,LHS_T,false,RHS_T> :
	    public Rcpp::VectorBase<REALSXP, false , Plus_Vector_Vector<REALSXP,false,LHS_T,false,RHS_T> > {
	public:
		typedef typename Rcpp::VectorBase<REALSXP,false,LHS_T> LHS_TYPE ;
		typedef typename Rcpp::VectorBase<REALSXP,false,RHS_T> RHS_TYPE ;

		typedef typename Rcpp::traits::Extractor<REALSXP, false, LHS_T>::type LHS_EXT ;
		typedef typename Rcpp::traits::Extractor<REALSXP, false, RHS_T>::type RHS_EXT ;

		Plus_Vector_Vector( const LHS_TYPE& lhs_, const RHS_TYPE& rhs_ ) :
			lhs(lhs_.get_ref()), rhs(rhs_.get_ref()){}

		inline double operator[]( int i ) const {
			return lhs[i] + rhs[i]  ;
		}

		inline int size() const { return lhs.size() ; }

	private:
		const LHS_EXT& lhs ;
		const RHS_EXT& rhs ;
	} ;







	template <int RTYPE, bool NA, typename T>
	class Plus_Vector_Primitive :
	    public Rcpp::VectorBase<RTYPE,true, Plus_Vector_Primitive<RTYPE,NA,T> > {
	public:
		typedef typename Rcpp::VectorBase<RTYPE,NA,T> VEC_TYPE ;
		typedef typename traits::storage_type<RTYPE>::type STORAGE ;

		typedef typename Rcpp::traits::Extractor< RTYPE, NA, T>::type EXT ;

		Plus_Vector_Primitive( const VEC_TYPE& lhs_, STORAGE rhs_ ) :
			lhs(lhs_.get_ref()), rhs(rhs_), rhs_na( Rcpp::traits::is_na<RTYPE>(rhs_) )
			{}

		inline STORAGE operator[]( int i ) const {
			if( rhs_na ) return rhs ;
			STORAGE x = lhs[i] ;
			return Rcpp::traits::is_na<RTYPE>(x) ? x : (x + rhs) ;
		}

		inline int size() const { return lhs.size() ; }

	private:
		const EXT& lhs ;
		STORAGE rhs ;
		bool rhs_na ;

	} ;
	// RTYPE = REALSXP
	template <bool NA, typename T>
	class Plus_Vector_Primitive<REALSXP,NA,T> :
	    public Rcpp::VectorBase<REALSXP,true, Plus_Vector_Primitive<REALSXP,NA,T> > {
	public:
		typedef typename Rcpp::VectorBase<REALSXP,NA,T> VEC_TYPE ;
		typedef typename Rcpp::traits::Extractor< REALSXP, NA, T>::type EXT ;

		Plus_Vector_Primitive( const VEC_TYPE& lhs_, double rhs_ ) :
			lhs(lhs_.get_ref()), rhs(rhs_)
			{}

		inline double operator[]( int i ) const {
			return rhs + lhs[i] ;
		}

		inline int size() const { return lhs.size() ; }

	private:
		const EXT& lhs ;
		double rhs ;
	} ;



	template <int RTYPE, typename T>
	class Plus_Vector_Primitive<RTYPE,false,T> : public Rcpp::VectorBase<RTYPE,false, Plus_Vector_Primitive<RTYPE,false,T> > {
	public:
		typedef typename Rcpp::VectorBase<RTYPE,false,T> VEC_TYPE ;
		typedef typename traits::storage_type<RTYPE>::type STORAGE ;

		typedef typename Rcpp::traits::Extractor< RTYPE, false, T>::type EXT ;

		Plus_Vector_Primitive( const VEC_TYPE& lhs_, STORAGE rhs_ ) :
			lhs(lhs_.get_ref()), rhs(rhs_), rhs_na( Rcpp::traits::is_na<RTYPE>(rhs_) ) {}

		inline STORAGE operator[]( int i ) const {
			return rhs_na ? rhs : (rhs + lhs[i] ) ;
		}

		inline int size() const { return lhs.size() ; }

	private:
		const EXT& lhs ;
		STORAGE rhs ;
		bool rhs_na ;
	} ;
	// RTYPE = REALSXP
	template <typename T>
	class Plus_Vector_Primitive<REALSXP,false,T> :
	    public Rcpp::VectorBase<REALSXP,false, Plus_Vector_Primitive<REALSXP,false,T> > {
	public:
		typedef typename Rcpp::VectorBase<REALSXP,false,T> VEC_TYPE ;

		typedef typename Rcpp::traits::Extractor< REALSXP, false, T>::type EXT ;

		Plus_Vector_Primitive( const VEC_TYPE& lhs_, double rhs_ ) :
			lhs(lhs_.get_ref()), rhs(rhs_) {}

		inline double operator[]( int i ) const {
			return rhs + lhs[i] ;
		}

		inline int size() const { return lhs.size() ; }

	private:
		const EXT& lhs ;
		double rhs ;
	} ;






	// Vector * nona(primitive)
	template <int RTYPE, bool NA, typename T>
	class Plus_Vector_Primitive_nona : public Rcpp::VectorBase<RTYPE,true, Plus_Vector_Primitive_nona<RTYPE,NA,T> > {
	public:
		typedef typename Rcpp::VectorBase<RTYPE,NA,T> VEC_TYPE ;
		typedef typename traits::storage_type<RTYPE>::type STORAGE ;
		typedef typename Rcpp::traits::Extractor< RTYPE, NA, T>::type EXT ;

		Plus_Vector_Primitive_nona( const VEC_TYPE& lhs_, STORAGE rhs_ ) :
			lhs(lhs_.get_ref()), rhs(rhs_)
			{}

		inline STORAGE operator[]( int i ) const {
			STORAGE x = lhs[i] ;
			return Rcpp::traits::is_na<RTYPE>(x) ? x : (x + rhs) ;
		}

		inline int size() const { return lhs.size() ; }

	private:
		const EXT& lhs ;
		STORAGE rhs ;

	} ;
	template <bool NA, typename T>
	class Plus_Vector_Primitive_nona<REALSXP,NA,T> :
	    public Rcpp::VectorBase<REALSXP,true, Plus_Vector_Primitive_nona<REALSXP,NA,T> > {
	public:
		typedef typename Rcpp::VectorBase<REALSXP,NA,T> VEC_TYPE ;
		typedef typename Rcpp::traits::Extractor<REALSXP, NA, T>::type EXT ;

		Plus_Vector_Primitive_nona( const VEC_TYPE& lhs_, double rhs_ ) :
			lhs(lhs_.get_ref()), rhs(rhs_)
			{}

		inline double operator[]( int i ) const {
			return rhs + lhs[i] ;
		}

		inline int size() const { return lhs.size() ; }

	private:
		const EXT& lhs ;
		double rhs ;

	} ;



	template <int RTYPE, typename T>
	class Plus_Vector_Primitive_nona<RTYPE,false,T> : public Rcpp::VectorBase<RTYPE,false, Plus_Vector_Primitive_nona<RTYPE,false,T> > {
	public:
		typedef typename Rcpp::VectorBase<RTYPE,false,T> VEC_TYPE ;
		typedef typename traits::storage_type<RTYPE>::type STORAGE ;

		typedef typename Rcpp::traits::Extractor< RTYPE, false, T>::type EXT ;

		Plus_Vector_Primitive_nona( const VEC_TYPE& lhs_, STORAGE rhs_ ) :
			lhs(lhs_.get_ref()), rhs(rhs_) {}

		inline STORAGE operator[]( int i ) const {
			return rhs + lhs[i] ;
		}

		inline int size() const { return lhs.size() ; }

	private:
		const EXT& lhs ;
		STORAGE rhs ;

	} ;
	// RTYPE = REALSXP
	template <typename T>
	class Plus_Vector_Primitive_nona<REALSXP,false,T> :
	    public Rcpp::VectorBase<REALSXP,false, Plus_Vector_Primitive_nona<REALSXP,false,T> > {
	public:
		typedef typename Rcpp::VectorBase<REALSXP,false,T> VEC_TYPE ;
		typedef typename Rcpp::traits::Extractor< REALSXP, false, T>::type EXT ;

		Plus_Vector_Primitive_nona( const VEC_TYPE& lhs_, double rhs_ ) :
			lhs(lhs_.get_ref()), rhs(rhs_) {}

		inline double operator[]( int i ) const {
			return rhs + lhs[i] ;
		}

		inline int size() const { return lhs.size() ; }

	private:
		const EXT& lhs ;
		double rhs ;

	} ;

}

template <int RTYPE,bool NA, typename T>
inline sugar::Plus_Vector_Primitive<RTYPE,NA,T>
operator+(
	const VectorBase<RTYPE,NA,T>& lhs,
	typename traits::storage_type<RTYPE>::type rhs
) {
	return sugar::Plus_Vector_Primitive<RTYPE,NA,T>( lhs, rhs ) ;
}


template <int RTYPE,bool NA, typename T>
inline sugar::Plus_Vector_Primitive< RTYPE , NA , T >
operator+(
	typename traits::storage_type<RTYPE>::type rhs,
	const VectorBase<RTYPE,NA,T>& lhs
) {
	return sugar::Plus_Vector_Primitive<RTYPE,NA, T >( lhs, rhs ) ;
}



template <int RTYPE,bool NA, typename T>
inline sugar::Plus_Vector_Primitive_nona<RTYPE,NA,T>
operator+(
	const VectorBase<RTYPE,NA,T>& lhs,
	typename sugar::NonaPrimitive< typename traits::storage_type<RTYPE>::type > rhs
) {
	return sugar::Plus_Vector_Primitive_nona<RTYPE,NA,T>( lhs, rhs ) ;
}

template <int RTYPE,bool NA, typename T>
inline sugar::Plus_Vector_Primitive_nona< RTYPE , NA , T >
operator+(
	typename sugar::NonaPrimitive< typename traits::storage_type<RTYPE>::type > rhs,
	const VectorBase<RTYPE,NA,T>& lhs
) {
	return sugar::Plus_Vector_Primitive_nona<RTYPE,NA, T >( lhs, rhs ) ;
}


template <int RTYPE,bool LHS_NA, typename LHS_T, bool RHS_NA, typename RHS_T>
inline sugar::Plus_Vector_Vector<
	RTYPE ,
	LHS_NA, LHS_T,
	RHS_NA, RHS_T
	>
operator+(
	const VectorBase<RTYPE,LHS_NA,LHS_T>& lhs,
	const VectorBase<RTYPE,RHS_NA,RHS_T>& rhs
) {
	return sugar::Plus_Vector_Vector<
		RTYPE,
		LHS_NA, LHS_T,
		RHS_NA, RHS_T
		>( lhs, rhs ) ;
}

}

#endif
