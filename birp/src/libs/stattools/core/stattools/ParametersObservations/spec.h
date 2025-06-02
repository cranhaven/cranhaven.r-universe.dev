//
// Created by madleina on 15.12.23.
//

#ifndef STATTOOLS_SPEC_H
#define STATTOOLS_SPEC_H

#include <array>
#include <cstddef>
#include <type_traits>
#include <tuple>

//------------------------------
// Enums
//------------------------------
namespace stattools {
enum class UpdateWeights { regular, irregular, geometricUniform, log10StatePosterior, powerStatePosterior };
enum class Constraints { unconstrained, sumOne, lengthOne };
enum class UpdateTypes { one, pair, joint };
enum class MarkovOrder { allDependent, allIndependent, different };

//------------------------------
// Hash
// No default, must always be provided
//------------------------------

template<size_t H> struct Hash {
	constexpr static size_t hash = H;
};

//------------------------------
// NumDim
// No default, must always be provided
//------------------------------

template<size_t> struct NumDim; // forward declaration

namespace impl {
struct NumDimBase {
	using def = NumDim<1>;
};
} // namespace impl

template<size_t N> struct NumDim : impl::NumDimBase {
	static constexpr size_t numDim = N;
};

//------------------------------
// Parallelize
//------------------------------

template<MarkovOrder> struct Parallelize; // forward declaration

namespace impl {
struct ParallelizeBase {
	using def = Parallelize<MarkovOrder::allDependent>;
};
} // namespace impl

template<MarkovOrder Order> struct Parallelize : impl::ParallelizeBase {
	static constexpr MarkovOrder markovOrder = Order;
	// only parallelize if there are independent elements
	static constexpr bool parallelize        = (Order != MarkovOrder::allDependent);
};

//------------------------------
// Weights
//------------------------------

template<size_t NumDim, UpdateWeights... W> struct Weights; // forward declaration

namespace impl {

template<size_t NumDim> struct WeightsBase {
private:
	template<size_t N, UpdateWeights... Values>
	struct _repeatHelper { // recursively repeats UpdateWeights::regular> NumDim times
		using type = typename _repeatHelper<N - 1, Values..., UpdateWeights::regular>::type;
	};

	template<UpdateWeights... Values> struct _repeatHelper<0, Values...> {
		using type = Weights<NumDim, Values...>;
	};

public:
	using def = typename _repeatHelper<NumDim>::type;
};
} // namespace impl

template<size_t NumDim, UpdateWeights... W> struct Weights : impl::WeightsBase<NumDim> {
	static_assert(sizeof...(W) == NumDim);
	static constexpr std::array<UpdateWeights, NumDim> weights = std::array{W...};
};

//------------------------------
// Constraints
//------------------------------

template<size_t> struct Unconstrained; // forward declaration

namespace impl {
struct ConstraintsBase {
	using def = Unconstrained<1>;
};
} // namespace impl

template<size_t NumUpdatesPerIteration = 1> struct Unconstrained : impl::ConstraintsBase {
	constexpr static Constraints constraint        = Constraints::unconstrained;
	constexpr static UpdateTypes updateType        = UpdateTypes::one;
	constexpr static size_t numUpdatesPerIteration = NumUpdatesPerIteration;
};

template<size_t AlongDim = 0, UpdateTypes UpdateType = UpdateTypes::pair, size_t NumUpdatesPerIteration = 1>
struct SumOne : impl::ConstraintsBase {
	static_assert(UpdateType != UpdateTypes::one); // can not be updated independently

	constexpr static Constraints constraint        = Constraints::sumOne;
	constexpr static size_t alongDim               = AlongDim;
	constexpr static UpdateTypes updateType        = UpdateType;
	constexpr static size_t numUpdatesPerIteration = NumUpdatesPerIteration;
};

template<size_t AlongDim = 0, UpdateTypes UpdateType = UpdateTypes::pair, size_t NumUpdatesPerIteration = 1>
struct LengthOne : impl::ConstraintsBase {
	static_assert(UpdateType != UpdateTypes::one); // can not be updated independently

	constexpr static Constraints constraint        = Constraints::lengthOne;
	constexpr static size_t alongDim               = AlongDim;
	constexpr static UpdateTypes updateType        = UpdateType;
	constexpr static size_t numUpdatesPerIteration = NumUpdatesPerIteration;
};

//------------------------------
// RJ-MCMC
//------------------------------

struct NoRJMCMC; // forward declaration

namespace impl {
struct RJMCMCBase {
	using def = NoRJMCMC;
};
} // namespace impl

struct NoRJMCMC : impl::RJMCMCBase {
	class Dummy {};
	constexpr static bool doRJMCMC = false;
	using specModelParameter       = Dummy;
	using proposalDistribution     = Dummy;
};

template<typename SpecModelParameter, typename ProposalDistribution> struct RJMCMC : impl::RJMCMCBase {
	constexpr static bool doRJMCMC = true;
	using specModelParameter       = SpecModelParameter;
	using proposalDistribution     = ProposalDistribution;
};

//------------------------------
// Helper functions
//------------------------------

namespace impl {

// tuple_element_index_helper and tuple_element_index:
// find the index in a tuple where the type matches a given type (FindMe)
// -> the type must be either exactly the same as FindMe
// -> or the type must be a derived class of FindMe
// inspired from https://devblogs.microsoft.com/oldnewthing/20200629-00/?p=103910

template<typename FindMe, typename Tuple> struct tuple_element_index_helper;

template<typename FindMe> struct tuple_element_index_helper<FindMe, std::tuple<>> {
	// base class: empty tuple -> not found
	static constexpr size_t value = 0;
};

template<typename FindMe, typename First, typename... Rest>
struct tuple_element_index_helper<FindMe, std::tuple<First, Rest...>> {
	// check if First and FindMe are of the same type or if FindMe is a base class of First
	// if yes -> set value = 0 (first element matches)
	// else: add one and search again in the rest of the tuple
	using RestTuple               = std::tuple<Rest...>;
	static constexpr size_t value = (std::is_base_of_v<FindMe, First> || std::is_same_v<FindMe, First>)
										? 0
										: 1 + tuple_element_index_helper<FindMe, RestTuple>::value;
};

template<typename FindMe, typename Tuple> struct tuple_element_index {
	static constexpr size_t value = tuple_element_index_helper<FindMe, Tuple>::value;
	static constexpr bool found   = value < std::tuple_size_v<Tuple>;
};

// findInTuple: two versions (with enable_if)
// 1) Tuple does not contain the type FindMe -> return the default specification of the type FindMe
// 2) Tuple does contain the type FindMe -> return the type of the Tuple at the index of the match

template<typename FindMe, typename Tuple, typename Enable = void> struct findInTuple;

template<typename FindMe, typename Tuple>
struct findInTuple<FindMe, Tuple, std::enable_if_t<!impl::tuple_element_index<FindMe, Tuple>::found>> {
	// FindMe not found -> define default type
	using type = typename FindMe::def;
};

template<typename FindMe, typename Tuple>
struct findInTuple<FindMe, Tuple, std::enable_if_t<impl::tuple_element_index<FindMe, Tuple>::found>> {
	// FindMe was found -> get the type of the tuple at that index
	constexpr static size_t ix = impl::tuple_element_index<FindMe, Tuple>::value;
	using type                 = std::tuple_element_t<ix, Tuple>;
};

} // namespace impl

//------------------------------
// Parameter specification
//------------------------------

template<typename Type, typename Hash, typename TypeBoxAbove, typename... Optionals> struct ParamSpec {
	// specification of compile-time features of TParameter

	// mandatory arguments
	using value_type             = Type;
	constexpr static size_t hash = Hash::hash;
	using typeBoxAbove           = TypeBoxAbove;

	// now parse optional arguments
	using Tuple = std::tuple<Optionals...>;

	constexpr static size_t numDim           = impl::findInTuple<impl::NumDimBase, Tuple>::type::numDim;
	constexpr static bool parallelize        = impl::findInTuple<impl::ParallelizeBase, Tuple>::type::parallelize;
	constexpr static MarkovOrder markovOrder = impl::findInTuple<impl::ParallelizeBase, Tuple>::type::markovOrder;
	using constraint                         = typename impl::findInTuple<impl::ConstraintsBase, Tuple>::type;
	constexpr static auto weights            = impl::findInTuple<impl::WeightsBase<numDim>, Tuple>::type::weights;
	using RJMCMC                             = typename impl::findInTuple<impl::RJMCMCBase, Tuple>::type;

	// it does not make sense to parallelize if all elements are dependent
	static_assert(!(parallelize && markovOrder == MarkovOrder::allDependent));
	// if there is a parallelization, make sure that the update type is 'one'
	static_assert(!(parallelize && constraint::updateType != UpdateTypes::one));
	// if there is an RJ-MCMC, make sure that the update type is 'one'
	static_assert((!RJMCMC::doRJMCMC) || (RJMCMC::doRJMCMC && constraint::updateType == UpdateTypes::one));
};

} // namespace stattools

#endif // STATTOOLS_SPEC_H
